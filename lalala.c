int x;
int y;
void* z;

void *fd4main(int i) {
  x = 2 + 5;
  y = ({
    if (1 == 2)
      7;
    8;
  });
  z = fd4main(x);
  ({
    int j = 4;
    j + x;
  });
  (int)((int*)z)[7];
  return 0;
}