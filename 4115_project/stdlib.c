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

#ifdef BUILD_TEST
int main()
{
	// TODO?
}
#endif
