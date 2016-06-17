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
			pragma(msg,name);
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

import dparse.types: NodeTypes, DeclarationTypes;
import dparse.ast: ExpressionNode, Declaration;

/* Every class type has a special field in it that determines whether
	 it can be derived to one class or another.

	 Of course we aren't allowed to access that field, because reasons,
	 so instead of switching on it, we have to just try each one of
	 all the types, until the cast returns not null.
*/

version(D_DOES_NOT_SUCK) {
	/*	This doesn't work because... reasons? */
	mixin template clone_types(Base, Types ...) {
		Base clone(T)(const T src) if(is(T == Base)) {
			if(src is null)
				return null;
		
			foreach(Type;Types) {
				Type dest = cast(Type)src;
				if(dest !is null)
					return cast(Base) clone(dest);
			}	
			assert(0,"couldn't clone");
		}
	}

	mixin clone_types!(ExpressionNode, NodeTypes);
	mixin clone_types!(Declaration, DeclarationTypes);

} else {

	ExpressionNode clone(T)(const T src) if(is(T == ExpressionNode)) {
		if(src is null)
			return null;
	
		foreach(Type;NodeTypes) {
			Type dest = cast(Type)src;
			if(dest !is null)
				return cast(ExpressionNode) clone(dest);
		}	
		assert(0,"couldn't clone expression");

	}

	Declaration clone(T)(const T src) if(is(T == Declaration)) {
		if(src is null)
			return null;
	
		foreach(Type;DeclarationTypes) {
			Type dest = cast(Type)src;
			if(dest !is null)
				return cast(Declaration) clone(dest);
		}	
		assert(0,"couldn't clone declaration");
	}
}

unittest {
	import dparse.ast;
	auto a = new AddExpression;
	auto b = cast(const)a;
	auto c = b.clone();
	static assert(is(typeof(a) == typeof(c)));
}
