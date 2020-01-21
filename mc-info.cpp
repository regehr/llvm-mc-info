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

// fixme remove unneeded ones
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.inc"
#include "llvm/MCA/CodeEmitter.h"
#include "llvm/MCA/Context.h"
#include "llvm/MCA/InstrBuilder.h"
#include "llvm/MCA/Pipeline.h"
#include "llvm/MCA/Stages/EntryStage.h"
#include "llvm/MCA/Stages/InstructionTables.h"
#include "llvm/MCA/Support.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"

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
    errs() << "TheTargetMachine can't emit a file of this type";
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
    errs() << "TheTargetMachine can't emit a file of this type";
    report_fatal_error("oops");
  }

  pass.run(*M);

  return Asm;
}

// heavily adapted from llvm-mca.cpp
void mcaInfo(SmallString<256> Asm, TargetMachine *TM) {
  Triple TheTriple(sys::getDefaultTargetTriple());
  auto TripleName = Triple::normalize(sys::getDefaultTargetTriple());
  auto MCPU = llvm::sys::getHostCPUName();

  auto BufferPtr = MemoryBuffer::getMemBufferCopy(Asm);
  
  std::unique_ptr<MCSubtargetInfo> STI(
      TM->getTarget().createMCSubtargetInfo(TripleName, MCPU, ""));

  if (!STI->isCPUStringValid(MCPU))
    report_fatal_error("invalid CPU string");

  if (!STI->getSchedModel().hasInstrSchedModel())
    report_fatal_error("unable to find scheduling model");

#if 0

  std::unique_ptr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
  assert(MRI && "Unable to create target register info!");

  MCTargetOptions MCOptions = InitMCTargetOptionsFromFlags();
  std::unique_ptr<MCAsmInfo> MAI(
      TheTarget->createMCAsmInfo(*MRI, TripleName, MCOptions));
  assert(MAI && "Unable to create target asm info!");

  MCObjectFileInfo MOFI;
  SourceMgr SrcMgr;

  // Tell SrcMgr about this buffer, which is what the parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*BufferPtr), SMLoc());

  MCContext Ctx(MAI.get(), MRI.get(), &MOFI, &SrcMgr);

  MOFI.InitMCObjectFileInfo(TheTriple, /* PIC= */ false, Ctx);

  std::unique_ptr<buffer_ostream> BOS;

  std::unique_ptr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());

  std::unique_ptr<MCInstrAnalysis> MCIA(
      TheTarget->createMCInstrAnalysis(MCII.get()));

  // Parse the input and create CodeRegions that llvm-mca can analyze.
  mca::AsmCodeRegionGenerator CRG(*TheTarget, SrcMgr, Ctx, *MAI, *STI, *MCII);
  Expected<const mca::CodeRegions &> RegionsOrErr = CRG.parseCodeRegions();
  if (!RegionsOrErr) {
    if (auto Err =
            handleErrors(RegionsOrErr.takeError(), [](const StringError &E) {
              WithColor::error() << E.getMessage() << '\n';
            })) {
      // Default case.
      WithColor::error() << toString(std::move(Err)) << '\n';
    }
    return 1;
  }
  const mca::CodeRegions &Regions = *RegionsOrErr;

  // Early exit if errors were found by the code region parsing logic.
  if (!Regions.isValid())
    return 1;

  if (Regions.empty()) {
    WithColor::error() << "no assembly instructions found.\n";
    return 1;
  }

  // Now initialize the output file.
  auto OF = getOutputStream();
  if (std::error_code EC = OF.getError()) {
    WithColor::error() << EC.message() << '\n';
    return 1;
  }

  unsigned AssemblerDialect = CRG.getAssemblerDialect();
  if (OutputAsmVariant >= 0)
    AssemblerDialect = static_cast<unsigned>(OutputAsmVariant);
  std::unique_ptr<MCInstPrinter> IP(TheTarget->createMCInstPrinter(
      Triple(TripleName), AssemblerDialect, *MAI, *MCII, *MRI));
  if (!IP) {
    WithColor::error()
        << "unable to create instruction printer for target triple '"
        << TheTriple.normalize() << "' with assembly variant "
        << AssemblerDialect << ".\n";
    return 1;
  }

  // Set the display preference for hex vs. decimal immediates.
  IP->setPrintImmHex(PrintImmHex);

  std::unique_ptr<ToolOutputFile> TOF = std::move(*OF);

  const MCSchedModel &SM = STI->getSchedModel();

  // Create an instruction builder.
  mca::InstrBuilder IB(*STI, *MCII, *MRI, MCIA.get());

  // Create a context to control ownership of the pipeline hardware.
  mca::Context MCA(*MRI, *STI);

  mca::PipelineOptions PO(MicroOpQueue, DecoderThroughput, DispatchWidth,
                          RegisterFileSize, LoadQueueSize, StoreQueueSize,
                          AssumeNoAlias, EnableBottleneckAnalysis);

  // Number each region in the sequence.
  unsigned RegionIdx = 0;

  std::unique_ptr<MCCodeEmitter> MCE(
      TheTarget->createMCCodeEmitter(*MCII, *MRI, Ctx));

  std::unique_ptr<MCAsmBackend> MAB(TheTarget->createMCAsmBackend(
      *STI, *MRI, InitMCTargetOptionsFromFlags()));

  for (const std::unique_ptr<mca::CodeRegion> &Region : Regions) {
    // Skip empty code regions.
    if (Region->empty())
      continue;

    // Don't print the header of this region if it is the default region, and
    // it doesn't have an end location.
    if (Region->startLoc().isValid() || Region->endLoc().isValid()) {
      TOF->os() << "\n[" << RegionIdx++ << "] Code Region";
      StringRef Desc = Region->getDescription();
      if (!Desc.empty())
        TOF->os() << " - " << Desc;
      TOF->os() << "\n\n";
    }

    // Lower the MCInst sequence into an mca::Instruction sequence.
    ArrayRef<MCInst> Insts = Region->getInstructions();
    mca::CodeEmitter CE(*STI, *MAB, *MCE, Insts);
    std::vector<std::unique_ptr<mca::Instruction>> LoweredSequence;
    for (const MCInst &MCI : Insts) {
      Expected<std::unique_ptr<mca::Instruction>> Inst =
          IB.createInstruction(MCI);
      if (!Inst) {
        if (auto NewE = handleErrors(
                Inst.takeError(),
                [&IP, &STI](const mca::InstructionError<MCInst> &IE) {
                  std::string InstructionStr;
                  raw_string_ostream SS(InstructionStr);
                  WithColor::error() << IE.Message << '\n';
                  IP->printInst(&IE.Inst, 0, "", *STI, SS);
                  SS.flush();
                  WithColor::note()
                      << "instruction: " << InstructionStr << '\n';
                })) {
          // Default case.
          WithColor::error() << toString(std::move(NewE));
        }
        return 1;
      }

      LoweredSequence.emplace_back(std::move(Inst.get()));
    }

    mca::SourceMgr S(LoweredSequence, PrintInstructionTables ? 1 : Iterations);

    if (PrintInstructionTables) {
      //  Create a pipeline, stages, and a printer.
      auto P = std::make_unique<mca::Pipeline>();
      P->appendStage(std::make_unique<mca::EntryStage>(S));
      P->appendStage(std::make_unique<mca::InstructionTables>(SM));
      mca::PipelinePrinter Printer(*P);

      // Create the views for this pipeline, execute, and emit a report.
      if (PrintInstructionInfoView) {
        Printer.addView(std::make_unique<mca::InstructionInfoView>(
            *STI, *MCII, CE, ShowEncoding, Insts, *IP));
      }
      Printer.addView(
          std::make_unique<mca::ResourcePressureView>(*STI, *IP, Insts));

      if (!runPipeline(*P))
        return 1;

      Printer.printReport(TOF->os());
      continue;
    }

    // Create a basic pipeline simulating an out-of-order backend.
    auto P = MCA.createDefaultPipeline(PO, S);
    mca::PipelinePrinter Printer(*P);

    if (PrintSummaryView)
      Printer.addView(
          std::make_unique<mca::SummaryView>(SM, Insts, DispatchWidth));

    if (EnableBottleneckAnalysis) {
      Printer.addView(std::make_unique<mca::BottleneckAnalysis>(
          *STI, *IP, Insts, S.getNumIterations()));
    }

    if (PrintInstructionInfoView)
      Printer.addView(std::make_unique<mca::InstructionInfoView>(
          *STI, *MCII, CE, ShowEncoding, Insts, *IP));

    if (PrintDispatchStats)
      Printer.addView(std::make_unique<mca::DispatchStatistics>());

    if (PrintSchedulerStats)
      Printer.addView(std::make_unique<mca::SchedulerStatistics>(*STI));

    if (PrintRetireStats)
      Printer.addView(std::make_unique<mca::RetireControlUnitStatistics>(SM));

    if (PrintRegisterFileStats)
      Printer.addView(std::make_unique<mca::RegisterFileStatistics>(*STI));

    if (PrintResourcePressureView)
      Printer.addView(
          std::make_unique<mca::ResourcePressureView>(*STI, *IP, Insts));

    if (PrintTimelineView) {
      unsigned TimelineIterations =
          TimelineMaxIterations ? TimelineMaxIterations : 10;
      Printer.addView(std::make_unique<mca::TimelineView>(
          *STI, *IP, Insts, std::min(TimelineIterations, S.getNumIterations()),
          TimelineMaxCycles));
    }

    if (!runPipeline(*P))
      return 1;

    Printer.printReport(TOF->os());

    // Clear the InstrBuilder internal state in preparation for another round.
    IB.clear();
  }

  TOF->keep();
#endif
}

// return 0 for success
int getInfo(Module *M, TargetMachine *TM) {
  outs() << "\n=========================================\n";
  M->print(outs(), nullptr);

  long Size = getCodeSize(M, TM);
  outs() << "code size = " << Size << " bytes\n";
  
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
