#include "llvm/ADT/APInt.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include <memory>
#include <utility>

using namespace llvm;

namespace {

// return 0 for success
int getInfo(Module *M) {
  outs() << "\n=========================================\n";
  M->print(outs(), nullptr);

  // generate object code
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto TargetTriple = sys::getDefaultTargetTriple();
  M->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TheTargetMachine =
      Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

  M->setDataLayout(TheTargetMachine->createDataLayout());

  auto Filename = "output.o";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }

  legacy::PassManager pass;
  auto FileType = CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(*M);
  dest.flush();

  outs() << "Wrote " << Filename << "\n";

  // analyze it using mca

  return 0;
}

const int W = 32;

LLVMContext C;
IRBuilder<> B(C);

struct BinOp {
  Instruction::BinaryOps Opcode;
  bool nsw, nuw, exact;
};

void test(const BinOp &Op) {
  auto M = std::make_unique<Module>("", C);
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

  //getInfo(M.get());
  
  // this is not good code but should be fine for a very small
  // number of instructions, as we have here
  while (!BB->empty()) {
    for (auto &I2 : *BB) {
      if (I2.hasNUses(0)) {
        I2.eraseFromParent();
        break;
      }
    }
  }
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

int main(void) {
  for (auto &Op : Ops)
    test(Op);
  return 0;
}
