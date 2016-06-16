module dparse.clone;

import std.traits: isIterable, FieldNameTuple, isAbstractClass;
import std.variant: VariantN; // eh

R clone(T: const R, R)(ref T src)
	if(!isIterable!T
		 // const is messed up
		 && !isAbstractClass!T
		 // these have to have a specialized cast to clone()
		 )
{

	static if(is(T == class) || is(T == struct)) {
		R dest;
		static if(is(T == class)) {
			dest = new R;
		}
		static if(__traits(compiles,dest = src)) {
			dest = src;
			return dest;
		} else {
		alias names = FieldNameTuple!T;
		foreach(name; names) {
			if(!is(typeof(__traits(getMember,src,name)) == immutable))
				__traits(getMember,dest,name) =
					cast(typeof(__traits(getMember,dest,name)))
					clone(__traits(getMember,src,name));
		}
		return dest;
		}
	} else {
		return cast(R)src;
	}
}

R[] clone(T: const R, R)(T[] src) {
	R[] dest;
	dest.length = src.length;
	foreach(i,e; src) {
		dest[i] = clone(e);
	}
	return dest;
}

auto derive_then_clone(Base,Types...)(Base src) {
}

import dparse.types: NodeTypes;
import dparse.ast: ExpressionNode;

ExpressionNode clone(T)(const T src) if(is(T == ExpressionNode)) {
	if(src is null)
		return null;
	
	foreach(Type;NodeTypes) {
		Type dest = cast(Type)src;
		if(dest !is null)
			return cast(ExpressionNode) clone(dest);
	}	
	assert(0,"couldn't clone");

}

unittest {
	import dparse.ast;
	auto a = new AddExpression;
	auto b = cast(const)a;
	auto c = b.clone();
	static assert(is(typeof(a) == typeof(c)));
}
