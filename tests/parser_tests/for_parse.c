int main() {
    int z = 0;
    for (int i = 0; i < 20; i = i + 1) {
        z = z + i;
    }

    for (;;) {
        z = z + 1;
    }

    for (z = 10; z < 20;) {
        z = z + 1;
    }
}