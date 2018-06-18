
#include <iostream>
#include <queue>

int main(int argc, char *argv[]) {
    std::queue<int> q;
    int x;
    while (std::cin >> x) {
        q.push(x);
    }
    int sum = 0;
    while (!q.empty()) {
        sum += q.front();
        q.pop();
    }
    std::cout << q.size();
    std::cout << sum;
    return 0;
}
