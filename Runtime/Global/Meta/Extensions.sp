package _

_funcExts: *[]::(*_Function, *_Interop_Vector<_Operand>, int32) = 0 as *void;
_blockExts: *[]::(*_Block, int32) = 0 as *void;
_labelExts: *[]::(*_Label, int32) = 0 as *void;
_instExts: *[]::(*_Instruction, *_Label, int32) = 0 as *void;
_initExts: *[]::(int32) = 0 as *void;

bool RegisterInterpreterExtension(
	onFunctionEnter: ::(*_Function, *_Interop_Vector<_Operand>, int32),
	onBlockEnter: ::(*_Block, int32) = 0 as *void,
	onLabelEnter: ::(*_Label, int32) = 0 as *void,
	onInstruction: ::(*_Instruction, *_Label, int32) = 0 as *void,
	onInit: ::(int32) = 0 as *void
)
{
	log "Registering Interpreter Extension";
	ret := false;
	if (_funcExts && onFunctionEnter)
	{
		_funcExts.Add(onFunctionEnter);
		ret = true;
	}

	if (_blockExts && onBlockEnter)
	{
		_blockExts.Add(onBlockEnter);
		ret = true;
	}

	if (_labelExts && onLabelEnter)
	{
		_labelExts.Add(onLabelEnter);
		ret = true;
	}

	if (_instExts && onInstruction)
	{
		_instExts.Add(onInstruction);
		ret = true;
	}

	if (_initExts && onInit)
	{
		_initExts.Add(onInstruction);
		ret = true;
	}

	return ret;
}