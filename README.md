# YAGL
### "Yet Another Graph Language"

This repository has developed a compiler for the YAGL language that was invented for the purposes of a project.
It was developed with the intention to make writing graph algorithms easier. 

Below is an example program written in YAGL. Please checkout the repo for more examples in 4115_project/final_demos.
```
import stdgraph.ygl

Graph example;
Node n(gen_name("1"));
Node n2(gen_name("2"));

example: + n + n2, n -> n2 -> n;

print_graph_lib(example);

String gen_name(String n) {
	String hidden = "ODE";
	String total = "";
	{
		String hidden = "N";
		total = total + hidden;
	}
	return total + hidden + n;
}
```
