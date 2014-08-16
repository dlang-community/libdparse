void foo()
{
	if (a) b();
	if (a) b(); else c();
	if (auto a = i == 10) b();
	if (int i = x < 100) b();
	switch (x) {}
	switch (x) { case 10: break; }
	switch (x) { case 10: break; default: break; }
	label: switch (x) {
	case 1: break label;
	case 2: goto default;
	case 3: goto label2;
	case 4: goto case 3;
	}
	while (true) { continue; }
	while (true) { continue onAndOn; }
	label2: while (true) { break label2; }
	do nothing(); while(false);
	do { nothing(); } while(false);
	for (int i = 0; i < 100; i++) {}
	for (; i < 100; i++) {}
	for (; i < 100;) {}
	for (int i = 0; ; i++) {}
	foreach (a; b) {}
	foreach (int a; b) {}
	foreach (a, b; c) {}
	foreach (a; c .. d) {}
	foreach (ref a; c .. d) {}
	foreach (inout a; c .. d) {}
	scope(failure) complain();
	scope(success) celebrate();
	scope(success) {celebrate();}
	synchronized (swimming) {}
	throw aParty!tonight;
	with (great.power) comes(great.responsibility);
	final switch (x) {}
}
