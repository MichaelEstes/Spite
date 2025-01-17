#pragma once

#include <filesystem>

#include "./LLVMContext.h"

struct LLVMCompile
{
	LLVMContext& llvmContext;

	llvm::Module& module;

	llvm::TargetMachine* targetMachine;

	LLVMCompile(LLVMContext& llvmContext) : llvmContext(llvmContext),
		module(llvmContext.module)
	{
		targetMachine = nullptr;
	}

	bool Initialize()
	{
		InitializeTarget();
		std::string targetTriple = BuildTargetTriple();
		eastl::string targetTripleStr = targetTriple.c_str();
		eastl::string targetTripleMsg = "LLVMCompile:Initialize Triple target: "
			+ targetTripleStr;
		Logger::Info(targetTripleMsg);
		module.setTargetTriple(targetTriple);

		std::string error;
		const llvm::Target* target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
		if (!target)
		{
			llvm::errs() << "Error: " << error << "\n";
			return false;
		}

		std::string cpu = "generic";
		std::string features = "";
		llvm::TargetOptions opt;
		targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, llvm::Reloc::PIC_);
		module.setDataLayout(targetMachine->createDataLayout());
		
		return true;
	}

	void InitializeTarget()
	{
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
	}

	llvm::Triple::ArchType GetTargetArch()
	{
		switch (config.arch)
		{
		case X64:
			return llvm::Triple::x86_64;
		case X86:
			return llvm::Triple::x86;
		case Arm32:
			return llvm::Triple::arm;
		case Arm64:
			return llvm::Triple::aarch64;
		case ArchInvalid:
			break;
		default:
			break;
		}

		return llvm::Triple::UnknownArch;
	}

	llvm::Triple::OSType GetTargetOS()
	{
		switch (config.os)
		{
		case Windows:
			return llvm::Triple::Win32;
		case Android:
		case Linux:
			return llvm::Triple::Linux;
		case Mac:
			return llvm::Triple::MacOSX;
		case Ios:
			return llvm::Triple::IOS;
		case OsInvalid:
			break;
		default:
			break;
		}

		return llvm::Triple::UnknownOS;
	}

	llvm::Triple::EnvironmentType GetTargetEnvironment() {
		switch (config.os)
		{
		case Windows:
			return llvm::Triple::MSVC;
		case Android:
			return llvm::Triple::Android;
		case Linux:
			return llvm::Triple::GNU;
		default:
			break;
		}

		return llvm::Triple::UnknownEnvironment;
	}

	llvm::Triple::VendorType GetTargetVendor()
	{
		switch (config.os)
		{
		case Windows:
		case Linux:
			return llvm::Triple::PC;
		case Ios:
			return llvm::Triple::Apple;
		default:
			break;
		}

		return llvm::Triple::UnknownVendor;
	}


	std::string BuildTargetTriple()
	{
		llvm::Triple triple;
		triple.setArch(GetTargetArch());
		triple.setOS(GetTargetOS());
		triple.setEnvironment(GetTargetEnvironment());
		triple.setVendor(GetTargetVendor());

		return triple.getTriple();
	}

	std::string GetDestExt()
	{
		switch (config.os)
		{
		case Windows:
			return ".obj";
		default:
			break;
		}

		return ".o";
	}

	std::filesystem::path CreateOutputPath(std::string ext)
	{
		std::string outputName(config.name.c_str());
		std::string outputFileName = outputName + GetDestExt();
		std::filesystem::path output = std::filesystem::current_path() / "Build" / outputFileName;

		std::string directory = output.parent_path().string();
		if (!directory.empty()) 
		{
			std::error_code ec = llvm::sys::fs::create_directories(directory);
			if (ec) 
			{
				llvm::errs() << "Error creating directories: " << ec.message() << "\n";
				Logger::FatalError("LLVMCompile:CreateOutputPath Unable to create build directory");
			}
		}

		return output;
	}

	bool Compile()
	{		
		std::filesystem::path output = CreateOutputPath(GetDestExt());

		std::error_code EC;
		llvm::raw_fd_ostream dest(output.string(), EC, llvm::sys::fs::OF_None);
		if (EC)
		{
			llvm::errs() << "Could not open file: " << EC.message() << "\n";
			return false;
		}

		llvm::legacy::PassManager pass;
		if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, llvm::CGFT_ObjectFile))
		{
			llvm::errs() << "TargetMachine can't emit a file of this type\n";
			return false;
		}

		pass.run(module);
		dest.flush();
		llvm::outs() << "Wrote " << output.string() << "\n";
		return true;
	}
};