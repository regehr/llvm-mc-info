#include "llvm/ADT/APInt.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include <memory>
#include <utility>

using namespace llvm;

namespace {

int getCodeSize(Module *M, TargetMachine *TM) {
  M->setDataLayout(TM->createDataLayout());
  SmallVector<char, 256> DotO;
  raw_svector_ostream dest(DotO);

  legacy::PassManager pass;
  if (TM->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    report_fatal_error("oops");
  }
  pass.run(*M);

  SmallVectorMemoryBuffer Buf(std::move(DotO));
  auto ObjOrErr = object::ObjectFile::createObjectFile(Buf);
  if (!ObjOrErr)
    report_fatal_error("createObjectFile() failed");
  object::ObjectFile *OF = ObjOrErr.get()->getBinary();  
  
  return 0;
}

SmallString<256> makeAssembly(Module *M, TargetMachine *TM) {
  M->setDataLayout(TM->createDataLayout());

  SmallString<256> Asm;
  raw_svector_ostream dest(Asm);

  legacy::PassManager pass;
  if (TM->addPassesToEmitFile(pass, dest, nullptr, CGFT_AssemblyFile)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    report_fatal_error("oops");
  }

  pass.run(*M);

  return Asm;
}

void mcaInfo(SmallString<256> Asm, TargetMachine *TM) {
}

// return 0 for success
int getInfo(Module *M, TargetMachine *TM) {
  outs() << "\n=========================================\n";
  M->print(outs(), nullptr);

  int Size = getCodeSize(M, TM);
  
  auto Asm = makeAssembly(M, TM);
  outs() << Asm;

  mcaInfo(Asm, TM);

  return 0;
}

const int W = 32;

struct BinOp {
  Instruction::BinaryOps Opcode;
  bool nsw, nuw, exact;
};

void test(const BinOp &Op, TargetMachine *TM) {
  LLVMContext C;
  IRBuilder<> B(C);

  auto M = std::make_unique<Module>("", C);
  M->setTargetTriple(sys::getDefaultTargetTriple());

  std::vector<Type *> T(2, Type::getIntNTy(C, W));
  FunctionType *FT = FunctionType::get(Type::getIntNTy(C, W), T, false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, "test", M.get());
  BasicBlock *BB = BasicBlock::Create(C, "", F);
  B.SetInsertPoint(BB);
  std::vector<Argument *> Args;
  for (auto &A : F->args())
    Args.push_back(&A);
  auto DL = M->getDataLayout();
  long Bits = 0, Cases = 0;

  auto I = BinaryOperator::Create(Op.Opcode, Args[0], Args[1]);
  if (Op.nsw)
    I->setHasNoSignedWrap();
  if (Op.nuw)
    I->setHasNoUnsignedWrap();
  if (Op.exact)
    I->setIsExact();
  B.Insert(I);
  auto R = B.CreateRet(I);

  if (verifyFunction(*F))
    report_fatal_error("verifyFunction");
  if (verifyModule(*M))
    report_fatal_error("verifyModule");

  getInfo(M.get(), TM);
}

std::vector<BinOp> Ops {
  { Instruction::Add, false, false, false },
  { Instruction::Add, true, false, false },
  { Instruction::Add, false, true, false },
  { Instruction::Add, true, true, false },
  { Instruction::Sub, false, false, false },
  { Instruction::Sub, true, false, false },
  { Instruction::Sub, false, true, false },
  { Instruction::Sub, true, true, false },
  { Instruction::Mul, false, false, false },
  { Instruction::Mul, true, false, false },
  { Instruction::Mul, false, true, false },
  { Instruction::Mul, true, true, false },
  { Instruction::UDiv, false, false, false },
  { Instruction::UDiv, false, false, true },
  { Instruction::SDiv, false, false, false },
  { Instruction::SDiv, false, false, true },
  { Instruction::URem, false, false, false },
  { Instruction::SRem, false, false, false },
  { Instruction::Shl, false, false, false },
  { Instruction::LShr, false, false, false },
  { Instruction::LShr, false, false, true },
  { Instruction::AShr, false, false, false },
  { Instruction::AShr, false, false, true },
  { Instruction::And, false, false, false },
  { Instruction::Or, false, false, false },
  { Instruction::Xor, false, false, false },
};
  
} // namespace

int main(int argc, char **argv) {
  InitLLVM X(argc, argv);

  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(sys::getDefaultTargetTriple(), Error);
  if (!Target) {
    errs() << Error;
    report_fatal_error("oops");
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TM = Target->createTargetMachine(sys::getDefaultTargetTriple(), CPU, Features, opt, RM);

  for (auto &Op : Ops)
    test(Op, TM);
  return 0;
}
