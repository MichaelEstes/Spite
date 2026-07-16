package _

import Math

state _string 
{
	[value]
	count: uint,
	mem: *byte
}

_string::delete {
	if (this.count) dealloc(this.mem);
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

_string::(c_str: *byte)
{
	this.mem = c_str;

	count := 0;
    while (this.mem[count]~) count += 1;
	this.count = count;
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
	if (this.mem == str.mem) return true;

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
	if (!this.count) return null;

	return this[this.count - 1];
}

string _string::Append(toAppend: string)
{
	if (!toAppend.count) return this;
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
	if (!toAppend.count) return;
	appended := this.Append(toAppend);
	delete this;
	this = appended;
}

string _string::PrecedingLast(char: byte)
{
	view := string(this);
	while(view.count > 0 && view[view.count - 1]~ != char)
		view.count -= 1;

	if (view.count > 0) view.count -= 1;
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

[Size]byte _string::ToFixed<Size>()
{
	ret := [Size]byte;
	zero_out_bytes(fixed ret, Size);
	count := Math.Min(Size, this.count);
	for (i .. count)
	{
		ret[i] = this[i]~;
	}

	return ret;
}

StringLineIterator _string::Lines()
{
	iter := StringLineIterator();
	iter.str = this;
	return iter;
}

string _string::Copy()
{
	buffer := ZeroedAllocator<byte>().Alloc(this.count + 1)[0];

	for (i .. this.count)
		buffer[i]~ = this[i]~;

	return string(this.count, buffer);
}

state StringLineIterator
{
	str: string
}

Iterator StringLineIterator::operator::in()
{
	return Iterator:{this.str[0], 0};
}

bool StringLineIterator::next(it: Iterator)
{
	start := it.current as *byte;
	end := this.str.Last()

	curr := start;
	while (curr~ != '\n' && curr != end)
	{
		curr += 1;
	}
	if (curr != end) curr += 1;

	it.current = curr;
	it.index = (curr - start) as int;
	return curr != end;
}

StringView StringLineIterator::current(it: Iterator)
{
	return StringView(it.index as uint, (it.current - it.index) as *byte);
}
