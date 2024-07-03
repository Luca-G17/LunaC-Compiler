int inf_for(int y) {
    for (;;) {
        if (y > 20) {
            return y;
        }
        y = y + 1;
    }
}

int inf_while(int y) {
    while (1) {
        if (y > 40) {
            return y;
        }
        y = y + 1;
    }
}

int main() {
    int x = 0;
    x = inf_for(0);
    x = inf_while(x);
}