/*
 *  Standard library functions in C for YAGL language
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *sconcat(char *s1, char *s2) {
	char *s3 = malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(s3, s1);
	strcat(s3, s2);
	return s3;
}


struct node {
	int id;  // for hash table
	char *val;
};

struct graph {
	int n_size;
	int n_pos;
	int e_pos;
	struct node **nodes;
	struct edge_list **edges;
};

struct edge {
	struct node *from_node;
	struct node *to_node;
	int val;
};

struct edge_list {
	struct edge *edge;
	struct edge_list *next_edge;
};

void copy_graph(struct graph *, struct graph *);
void insert_node(struct graph *, struct node*);
struct edge_list *new_edge_list(struct node *, struct node *, int);
void insert_into_edge_list(struct edge_list *, struct edge_list *);
int g_contain_n(struct graph *, struct node *);

void insert_edge(struct graph *g, struct node *from, struct node *to, int v) {
	if (!g_contain_n(g, from) || !g_contain_n(g, to)) {
		printf("Nodes don't exist in given graph.\n");
		return;
	}

	// first find where to insert
	struct edge_list *list;
	int pos = g->e_pos;
	int from_id = from->id;
	for (int e = 0; e < g->e_pos; e++) {
		struct edge_list *at_e = g->edges[e];
		if (at_e != NULL) {
			struct edge *edge = at_e->edge;
			if (edge && edge->from_node && edge->from_node->id == from_id) {
				pos = e;
			}
		}
	}

	// Next: insert
	struct edge_list *holder = NULL;
	if (pos == g->e_pos) {
		g->e_pos ++;
		holder = new_edge_list(from, to, v);
		g->edges[pos] = holder;
		return;
	}
	holder = g->edges[pos];
	struct edge_list *new_el = new_edge_list(from, to, v);
	insert_into_edge_list(holder, new_el);
}

void insert_into_edge_list(struct edge_list *e, struct edge_list *new) {
	struct edge_list *curr = e;
	while (curr->next_edge) {
		curr = curr->next_edge;
	}
	curr->next_edge = new;
}

struct edge_list *new_edge_list(struct node *from, struct node *to, int v) {
	struct edge_list *e = malloc(sizeof(struct edge_list));
	struct edge *edge = malloc(sizeof(struct edge));
	edge->from_node = from;
	edge->to_node = to;
	edge->val = v;
	e->edge = edge;
	e->next_edge = NULL;
	return e;
}
struct graph *make_graph(int n_size) {
	struct graph *g = malloc(sizeof(struct graph));
	g->n_size = n_size;
	g->n_pos = 0;
	g->e_pos = 0;
	g->nodes = (struct node **) malloc(sizeof(struct node *) * g->n_size);
	
	struct edge_list **edges = malloc(g->n_size * sizeof(struct edge_list *));
	g->edges = edges;
	return g;
}

void insert_node(struct graph *g, struct node *n) {
	if (g_contain_n(g, n)) {
		printf("Already inserted into graph.\n");
		return;
	}
	if (g->n_pos < g->n_size) {
		struct node **nodes = g->nodes;
		nodes[g->n_pos++] = n;
	} else {
		struct graph *g_new = make_graph(g->n_size*2); // double space
		copy_graph(g, g_new); // copy values of g to g_new
		memcpy(g, g_new, sizeof(*g)); // copy g_new to g in memory
		free(g_new); // clean up space
		insert_node(g, n); // insert new node
	}
	
}

void copy_graph(struct graph *g, struct graph *g_new) {
	for (int i = 0; i < g->n_size; i++) {
		insert_node(g_new, g->nodes[i]);
	}
	for (int n = 0; n < g->e_pos; n++) {
		struct edge_list *e = g->edges[n];
		while (e != NULL && e->edge != NULL) {
			struct edge *edge = e->edge;
			e = e->next_edge;
			insert_edge(g_new, edge->from_node, edge->to_node, edge->val);
		}
	}
}
int g_contain_n(struct graph *g, struct node *n) {
	int on = 0;
	while (on < g->n_pos) {
		struct node *x = g->nodes[on++];
		if (x->id == n->id)
			return 1;
	}
	return 0;
}

struct edge *g_contain_e(struct graph *g, struct edge *ed) {
	for (int n = 0; n < g->e_pos; n++) {
		struct edge_list *e = g->edges[n];
		while (e != NULL && e->edge != NULL) {
			struct edge *edge = e->edge;
			if (edge == ed)
				return edge;
			e = e->next_edge;
		}
	}
	return NULL;
}

static int id = 0;
struct node *make_node(char *val) {
	struct node *n = malloc(sizeof(struct node));
	n->id = id++;
	n->val = val;
	return n;
}

void print_graph(struct graph *g) {
	printf("============== Graph Print ===============\n");
	printf("\tStats: \t%d\t%d\t%d\n", g->n_size, g->n_pos, g->e_pos);
	int iter = 1;
	int per_line = 2;
	for (int n = 0; n < g->n_pos; n++)
		printf("Node (%d): %s%s", g->nodes[n]->id, g->nodes[n]->val,
				iter++ % per_line == 0 ? "\n": 
				n == g->n_pos - 1 ? "" : " --- ");
	if (g->n_pos == 0)
		printf("---- No Nodes in Graph ----\n");
	printf("\n");
	for (int n = 0; n < g->e_pos; n++) {
		struct edge_list *e = g->edges[n];
		while (e != NULL && e->edge != NULL) {
			struct edge *edge = e->edge;
			printf("Edge ([%d]/%s, [%d]/%s): %d\n", edge->from_node->id, 
					edge->from_node->val, edge->to_node->id, 
					edge->to_node->val, edge->val);
			e = e->next_edge;
		}
	}
	if (g->e_pos == 0)
		printf("---- No Edges in Graph ----\n");
	printf("============ End Graph Print =============\n");

	/* Testing stuff DELETE LATER */
	struct node *r = make_node("Pittsburgh");
	struct node *r2 = make_node("New York City");
	insert_node(g, r);
	insert_node(g, r2);
	insert_edge(g, r, r2, 5);
}

#ifdef BUILD_TEST
int main()
{
	// TODO?
}
#endif
