int main() {
    int x = 0;
    for (int i = 1; i <= 100 / 5; i += 1) {
        x += (5 * i);
    }
    for (int i = 1; i <= 100 / 9; i += 1) {
        x += (9 * i);
    }
    for (int i = 1; i <= 100 / 45; i += 1) {
        x -= (45 * i);
    }
}