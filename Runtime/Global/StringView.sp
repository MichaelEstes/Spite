package _

state StringView 
{
	count: uint,
	view: *byte
}

StringView::(str: string)
{
	this.count = str.count;
	this.view = str[0];
}

StringView::(count: uint, view: *byte)
{
	this.count = count;
	this.view = view;
}

string StringView::log()
{
	return string(this.count, this.view);
}

*byte StringView::operator::[](index: uint)
{
	return this.view[index];
}

bool StringView::operator::==(str: string)
{
	if (this.count != str.count) return false;

	for (i .. this.count)
		if (this[i]~ != str[i]~) return false;

	return true;
}

bool StringView::operator::!=(str: string)
{
	if (this.count != str.count) return true;

	for (i .. this.count)
		if (this[i]~ != str[i]~) return true;

	return false;
}

bool StringView::operator::!()
{
	return !this.count;
}

*byte StringView::Last()
{
	return this[this.count - 1];
}

bool StringView::Increment()
{
	if (!this.count) return false;

	this.count -= 1;
	this.view += 1;
	return true;
}

bool StringView::Decrement()
{
	if (!this.count) return false;

	this.count -= 1;
	return true;
}

StringView::Advance(count: uint)
{
	this.count -= count;
	this.view += count;
}

bool StringView::StartsWith(str: string)
{
	if (str.count > this.count) return false;

	for (i .. str.count)
	{
		if (this[i]~ != str[i]~) return false;
	}

	return true;
}