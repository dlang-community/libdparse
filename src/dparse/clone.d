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
	pragma(msg,"cloning ",R," ",T);

	static if(is(T == class) || is(T == struct)) {
		pragma(msg,"structured");
		R dest;
		static if(is(T == class)) {
			pragma(msg,"class");
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
		pragma(msg,"huh?");
		return cast(R)src;
	}
}

R[] clone(T: const R, R)(T[] src) {
	pragma(msg,"cloning array ",T);
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

ExpressionNode clone()(const ExpressionNode src) {
	foreach(Type;NodeTypes) {
		Type dest = cast(Type)src;
		if(dest !is null)
			return cast(ExpressionNode) clone(dest);
	}
	pragma(msg,"no type derived?");
	assert(0);

}

unittest {
	import dparse.ast;

	auto a = new AddExpression;
	auto b = cast(const)a;
	auto c = b.clone();
	pragma(msg,typeof(c));
}
