package _

_funcExts: *[]::(*_Function, *_Interop_Vector<_Operand>) = null;
_blockExts: *[]::(*_Block) = null;
_labelExts: *[]::(*_Label) = null;
_instExts: *[]::(*_Instruction, *_Label) = null;

bool RegisterInterpreterExtension(
	onFunctionEnter: ::(*_Function, *_Interop_Vector<_Operand>),
	onBlockEnter: ::(*_Block) = null,
	onLabelEnter: ::(*_Label) = null,
	onInstruction: ::(*_Instruction, *_Label) = null
)
{
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

	return ret;
}