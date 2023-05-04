unittest { i++; }
unittest { ++i; }
unittest { i--; }
unittest { --i; }
unittest { i -= +j; }
unittest { i += -j; }
unittest { i -= ~j; }
unittest { i += !j; }
unittest { i -= *j; }
unittest { i += &j; }
unittest { i = cast(int)j; }
unittest { x = assert(0); }
unittest { x = throw y; }
unittest { x = delete f; }
unittest { x = new F(); }
unittest {
	*&~(+1 | (int).init).new T(2);
}