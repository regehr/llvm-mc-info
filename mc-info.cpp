#include "llvm/ADT/APInt.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Support/KnownBits.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>
#include <iomanip>

using namespace llvm;

namespace {

void getInfo(Module *M) {
}

const int W = 32;

LLVMContext C;
IRBuilder<> B(C);

struct BinOp {
  Instruction::BinaryOps Opcode;
  bool nsw, nuw, exact;
};

void test(const BinOp &Op) {
  auto M = make_unique<Module>("", C);
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
  I->setHasNoSignedWrap(Op.nsw);
  I->setHasNoUnsignedWrap(Op.nuw);
  I->setIsExact(Op.exact);
  B.Insert(I);
  auto R = B.CreateRet(I);
  
  if (false) {
    M->print(errs(), nullptr);
  }

  getInfo(M.get());
  
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

  outs() << Instruction::getOpcodeName(Op.Opcode) << " ";
  if (Op.nsw)
    outs() << "nsw ";
  if (Op.nuw)
    outs() << "nuw ";
  if (Op.exact)
    outs() << "exact ";
  outs() << "\n";
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
