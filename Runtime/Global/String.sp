package _

state _string 
{
	count: uint,
	str: *byte
}

_string::delete {
	delete this.str;
}

_string::(count: uint, str: *byte)
{
	this.count = count;
	this.str = str;
}

*byte _string::operator::[](index: uint)
{
	return this.str[index];
}

string _string::operator::+(toAppend: string)
{
	return this.Append(toAppend);
}

bool _string::operator::!()
{
	return !this.count;
}

string _string::Append(toAppend: string)
{
	totalCount := this.count + toAppend.count;
	buffer := alloc(totalCount);
	
	for (i .. this.count)
		buffer[i]~ = this[i]~;
	
	for (j .. toAppend.count)
		buffer[i + j]~ = toAppend[j]~;

	return _string(totalCount, buffer);
}

_string::AppendIn(toAppend: string)
{
	appended := this.Append(toAppend);
	delete this;
	this = appended;
}