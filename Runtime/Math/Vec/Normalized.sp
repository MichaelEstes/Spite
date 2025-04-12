package Vec

// Wrapper state to make it explicit when a vector is normalized
state Norm<V>
{
	vec: V
}

Norm::(vec: V)
{
	this.vec = vec;
}