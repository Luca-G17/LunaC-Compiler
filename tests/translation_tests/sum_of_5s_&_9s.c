int main() {
    int x = 0;
    for (int i = 1; i <= 100 / 5; i = i + 1) {
        x = x + (5 * i);
    }
    for (int i = 1; i <= 100 / 9; i = i + 1) {
        x = x + (9 * i);
    }
    for (int i = 1; i <= 100 / 45; i = i + 1) {
        x = x - (45 * i);
    }
}