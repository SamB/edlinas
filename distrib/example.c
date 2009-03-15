extern pks();

main()
{
	int k,n,m;

	printf("\n Enter n: ");
	scanf("%d", &n);
	printf("\n\n Enter k: ");
	scanf("%d", &k);
	m = pks(n,k);
	printf("\n\n C(n,k)= %d\n\n",m);
}
