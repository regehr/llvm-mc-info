#include "BottleneckAnalysis.h"
#include "CodeRegion.h"
#include "CodeRegionGenerator.h"
#include "DispatchStatistics.h"
#include "InstructionInfoView.h"
#include "PipelinePrinter.h"
#include "RegisterFileStatistics.h"
#include "ResourcePressureView.h"
#include "RetireControlUnitStatistics.h"
#include "SchedulerStatistics.h"
#include "SummaryView.h"
#include "TimelineView.h"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCParser/AsmLexer.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.inc"
#include "llvm/MCA/CodeEmitter.h"
#include "llvm/MCA/Context.h"
#include "llvm/MCA/InstrBuilder.h"
#include "llvm/MCA/Pipeline.h"
#include "llvm/MCA/Stages/EntryStage.h"
#include "llvm/MCA/Stages/InstructionTables.h"
#include "llvm/MCA/Support.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Passes/PassBuilder.h"
//#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <memory>
#include <utility>

using namespace llvm;

namespace {

long getCodeSize(Module *M, TargetMachine *TM) {
  M->setDataLayout(TM->createDataLayout());
  SmallVector<char, 256> DotO;
  raw_svector_ostream dest(DotO);

  legacy::PassManager pass;
  if (TM->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) {
    errs() << "Target machine can't emit a file of this type";
    report_fatal_error("oops");
  }
  pass.run(*M);

  SmallVectorMemoryBuffer Buf(std::move(DotO));
  auto ObjOrErr = object::ObjectFile::createObjectFile(Buf);
  if (!ObjOrErr)
    report_fatal_error("createObjectFile() failed");
  object::ObjectFile *OF = ObjOrErr.get().get();
  auto SecList = OF->sections();
  long Size = 0;
  for (auto &S : SecList) {
    if (S.isText())
      Size += S.getSize();
  }
  if (Size > 0)
    return Size;
  else
    report_fatal_error("no text segment found");
}

SmallString<256> makeAssembly(Module *M, TargetMachine *TM) {
  M->setDataLayout(TM->createDataLayout());

  SmallString<256> Asm;
  raw_svector_ostream dest(Asm);

  legacy::PassManager pass;
  if (TM->addPassesToEmitFile(pass, dest, nullptr, CGFT_AssemblyFile)) {
    errs() << "Target machine can't emit a file of this type";
    report_fatal_error("oops");
  }

  pass.run(*M);

  return Asm;
}

#if 0
std::string Trip = "x86_64";
std::string CPU = "skylake";
#else
std::string Trip = "aarch64";
std::string CPU = "apple-a12";
#endif

// heavily adapted from llvm-mca.cpp
void mcaInfo(SmallString<256> Asm, TargetMachine *TM) {
  Triple TheTriple(Trip);
  auto TripleName = Triple::normalize(Trip);

  auto BufferPtr = MemoryBuffer::getMemBufferCopy(Asm);

  std::unique_ptr<MCSubtargetInfo> STI(
      TM->getTarget().createMCSubtargetInfo(TripleName, CPU, ""));

  if (!STI->isCPUStringValid(CPU))
    report_fatal_error("invalid CPU string");

  if (!STI->getSchedModel().hasInstrSchedModel())
    report_fatal_error("unable to find scheduling model");

  std::unique_ptr<MCRegisterInfo> MRI(
      TM->getTarget().createMCRegInfo(TripleName));
  if (!MRI)
    report_fatal_error("Unable to create target register info!");

  MCTargetOptions MCOptions = InitMCTargetOptionsFromFlags();

  std::unique_ptr<MCAsmInfo> MAI(
      TM->getTarget().createMCAsmInfo(*MRI, TripleName, MCOptions));
  if (!MAI)
    report_fatal_error("Unable to create target asm info!");

  MCObjectFileInfo MOFI;
  SourceMgr SrcMgr;

  SrcMgr.AddNewSourceBuffer(std::move(BufferPtr), SMLoc());

  MCContext Ctx(MAI.get(), MRI.get(), &MOFI, &SrcMgr);

  MOFI.InitMCObjectFileInfo(TheTriple, /* PIC= */ false, Ctx);

  std::unique_ptr<buffer_ostream> BOS;

  std::unique_ptr<MCInstrInfo> MCII(TM->getTarget().createMCInstrInfo());

  std::unique_ptr<MCInstrAnalysis> MCIA(
      TM->getTarget().createMCInstrAnalysis(MCII.get()));

  mca::AsmCodeRegionGenerator CRG(TM->getTarget(), SrcMgr, Ctx, *MAI, *STI,
                                  *MCII);

  // Parse the input and create CodeRegions that llvm-mca can analyze.
  Expected<const mca::CodeRegions &> RegionsOrErr = CRG.parseCodeRegions();
  if (!RegionsOrErr)
    report_fatal_error("cannot parse input regions");
  const mca::CodeRegions &Regions = *RegionsOrErr;

  // Early exit if errors were found by the code region parsing logic.
  if (!Regions.isValid())
    report_fatal_error("invalid regions");

  if (Regions.empty())
    report_fatal_error("no instructions found");

  unsigned AssemblerDialect = CRG.getAssemblerDialect();
  std::unique_ptr<MCInstPrinter> IP(TM->getTarget().createMCInstPrinter(
      Triple(TripleName), AssemblerDialect, *MAI, *MCII, *MRI));
  if (!IP)
    report_fatal_error("cannot create instruction printer");

  const MCSchedModel &SM = STI->getSchedModel();

  // Create an instruction builder.
  mca::InstrBuilder IB(*STI, *MCII, *MRI, MCIA.get());

  // Create a context to control ownership of the pipeline hardware.
  mca::Context MCA(*MRI, *STI);

  // gotta be a better way to do this
  mca::PipelineOptions PO(0, 0, 0, 0, 0, 0, true, false);

  // Number each region in the sequence.
  unsigned RegionIdx = 0;

  std::unique_ptr<MCCodeEmitter> MCE(
      TM->getTarget().createMCCodeEmitter(*MCII, *MRI, Ctx));

  std::unique_ptr<MCAsmBackend> MAB(TM->getTarget().createMCAsmBackend(
      *STI, *MRI, InitMCTargetOptionsFromFlags()));

  int Count = 0;

  for (const std::unique_ptr<mca::CodeRegion> &Region : Regions) {

    // Skip empty code regions
    if (Region->empty())
      continue;

    outs() << "non-empty region " << Count++ << "\n";

    // Don't print the header of this region if it is the default region, and
    // it doesn't have an end location.
    if (Region->startLoc().isValid() || Region->endLoc().isValid()) {
      outs() << "\n[" << RegionIdx++ << "] Code Region";
      StringRef Desc = Region->getDescription();
      if (!Desc.empty())
        outs() << " - " << Desc;
      outs() << "\n\n";
    }

    // Lower the MCInst sequence into an mca::Instruction sequence.
    ArrayRef<MCInst> Insts = Region->getInstructions();
    mca::CodeEmitter CE(*STI, *MAB, *MCE, Insts);
    std::vector<std::unique_ptr<mca::Instruction>> LoweredSequence;
    for (const MCInst &MCI : Insts) {
      Expected<std::unique_ptr<mca::Instruction>> Inst =
          IB.createInstruction(MCI);
      if (!Inst)
        report_fatal_error("cannot create MCI");

      LoweredSequence.emplace_back(std::move(Inst.get()));
    }

    mca::SourceMgr S(LoweredSequence, 0);

    // Create a basic pipeline simulating an out-of-order backend.
    auto P = MCA.createDefaultPipeline(PO, S);
    mca::PipelinePrinter Printer(*P);

#if 0
    
    if (true)
      Printer.addView(
          std::make_unique<mca::SummaryView>(SM, Insts, 0));

    if (true) {
      Printer.addView(std::make_unique<mca::BottleneckAnalysis>(
          *STI, *IP, Insts, S.getNumIterations()));
    }

    if (true)
      Printer.addView(std::make_unique<mca::InstructionInfoView>(
          *STI, *MCII, CE, true, Insts, *IP));

    if (true)
      Printer.addView(std::make_unique<mca::DispatchStatistics>());

    if (true)
      Printer.addView(std::make_unique<mca::SchedulerStatistics>(*STI));

    if (true)
      Printer.addView(std::make_unique<mca::RetireControlUnitStatistics>(SM));

    if (true)
      Printer.addView(std::make_unique<mca::RegisterFileStatistics>(*STI));

    if (true)
      Printer.addView(
          std::make_unique<mca::ResourcePressureView>(*STI, *IP, Insts));

    if (true) {
      unsigned TimelineIterations = 10;
      Printer.addView(std::make_unique<mca::TimelineView>(
          *STI, *IP, Insts, std::min(TimelineIterations, S.getNumIterations()),
          80));
    }

#endif

    Expected<unsigned> Cycles = P->run();
    if (!Cycles)
      report_fatal_error(toString(Cycles.takeError()));
    outs() << "cycles = " << Cycles.get() << "\n";

    Printer.printReport(outs());

    // Clear the InstrBuilder internal state in preparation for another round.
    IB.clear();
  }
}

// return 0 for success
int getInfo(Module *M, TargetMachine *TM) {
  outs() << "\n=========================================\n";
  M->print(outs(), nullptr);

  long Size = getCodeSize(M, TM);
  outs() << "code size = " << Size << " bytes\n";

  auto Asm = makeAssembly(M, TM);
  // outs() << Asm;
  mcaInfo(Asm, TM);

  return 0;
}

const int W = 32;

struct BinOp {
  Instruction::BinaryOps Opcode;
  bool nsw, nuw, exact;
};

void optimizeModule(llvm::Module *M) {
  auto FPM = new legacy::FunctionPassManager(M);
  PassManagerBuilder PB;
  PB.OptLevel = 2;
  PB.SizeLevel = 0;
  // PB.populateFunctionPassManager(*FPM);
  PB.populateModulePassManager(*FPM);
  FPM->doInitialization();
  for (Function &F : *M) {
    outs() << "optimizing " << F.getName() << "\n";
    FPM->run(F);
  }
  FPM->doFinalization();
  delete FPM;
}

void test(const BinOp &Op, TargetMachine *TM) {
  LLVMContext C;
  IRBuilder<> B(C);

  auto M = std::make_unique<Module>("", C);
  M->setTargetTriple(Trip);

  std::vector<Type *> T(2, Type::getIntNTy(C, W));
  FunctionType *FT = FunctionType::get(Type::getIntNTy(C, W), T, false);
  Function *F =
      Function::Create(FT, Function::ExternalLinkage, "test", M.get());
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

  optimizeModule(M.get());

  getInfo(M.get(), TM);
}

std::vector<BinOp> Ops{
    {Instruction::Add, false, false, false},
    {Instruction::Add, true, false, false},
    {Instruction::Add, false, true, false},
    {Instruction::Add, true, true, false},
    {Instruction::Sub, false, false, false},
    {Instruction::Sub, true, false, false},
    {Instruction::Sub, false, true, false},
    {Instruction::Sub, true, true, false},
    {Instruction::Mul, false, false, false},
    {Instruction::Mul, true, false, false},
    {Instruction::Mul, false, true, false},
    {Instruction::Mul, true, true, false},
    {Instruction::UDiv, false, false, false},
    {Instruction::UDiv, false, false, true},
    {Instruction::SDiv, false, false, false},
    {Instruction::SDiv, false, false, true},
    {Instruction::URem, false, false, false},
    {Instruction::SRem, false, false, false},
    {Instruction::Shl, false, false, false},
    {Instruction::LShr, false, false, false},
    {Instruction::LShr, false, false, true},
    {Instruction::AShr, false, false, false},
    {Instruction::AShr, false, false, true},
    {Instruction::And, false, false, false},
    {Instruction::Or, false, false, false},
    {Instruction::Xor, false, false, false},
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
  auto Target = TargetRegistry::lookupTarget(Trip, Error);
  if (!Target) {
    errs() << Error;
    report_fatal_error("oops");
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TM = Target->createTargetMachine(Trip, CPU, Features, opt, RM);

  for (auto &Op : Ops)
    test(Op, TM);
  return 0;
}
