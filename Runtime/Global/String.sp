package _

state _string 
{
	count: uint,
	str: *byte
}

*byte _string::operator::[](index: uint)
{
	return this.str[index];
}