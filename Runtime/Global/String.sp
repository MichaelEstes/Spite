package _

state _string 
{
	[value]
	count: uint,
	mem: *byte
}

_string::delete {
	delete this.mem;
}

_string::(str: string)
{
	this.count = str.count;
	this.mem = str.mem;
}

_string::(count: uint, mem: *byte)
{
	this.count = count;
	this.mem = mem;
}

*byte _string::operator::[](index: uint)
{
	return this.mem[index];
}

string _string::operator::+(toAppend: string)
{
	return this.Append(toAppend);
}

bool _string::operator::==(str: string)
{
	if (this.count != str.count) return false;

	for (i .. this.count)
		if (this[i]~ != str[i]~) return false;

	return true;
}

bool _string::operator::!=(str: string)
{
	if (this.count != str.count) return true;

	for (i .. this.count)
		if (this[i]~ != str[i]~) return true;

	return false;
}

bool _string::operator::!()
{
	return !this.count;
}

*byte _string::Last()
{
	return this[this.count - 1];
}

string _string::Append(toAppend: string)
{
	totalCount := this.count + toAppend.count;
	buffer := ZeroedAllocator<byte>().Alloc(totalCount + 1)[0];

	for (i .. this.count)
		buffer[i]~ = this[i]~;
	
	for (j .. toAppend.count)
		buffer[i + j]~ = toAppend[j]~;

	return string(totalCount, buffer);
}

_string::AppendIn(toAppend: string)
{
	appended := this.Append(toAppend);
	delete this;
	this = appended;
}

string _string::PrecedingLast(char: byte)
{
	view := string(this);
	while(view.count > 1 && view[view.count]~ != char)
		view.count -= 1;
	
	view.count -= 1;
	return view;
}

bool _string::StartsWith(str: string)
{
	if (str.count > this.count) return false;

	for (i .. str.count)
	{
		if (this[i]~ != str[i]~) return false;
	}

	return true;
}

string _string::Copy()
{
	buffer := ZeroedAllocator<byte>().Alloc(this.count + 1)[0];

	for (i .. this.count)
		buffer[i]~ = this[i]~;

	return string(this.count, buffer);
}