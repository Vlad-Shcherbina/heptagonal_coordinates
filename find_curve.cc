#include <iostream>
#include <fstream>
#include <array>
#include <vector>
#include <cassert>
#include <algorithm>
#include <set>
#include <utility>
#include <string>
#include <iterator>
#include <functional>

using namespace std;


#define debug(x) cerr << #x " = " << (x) << endl;
#define debug2(x, y) cerr << #x " = " << (x) << ", " #y " = " << y << endl;


int rotate(const array<int, 7> adj, int x, int angle) {
    assert(adj.size() + angle >= 0);
    auto p = find(adj.begin(), adj.end(), x);
    assert(p != adj.end());
    return adj[(p - adj.begin() + adj.size() + angle) % adj.size()];
}

vector<array<int, 7>> adjacent;


vector<int> build_fibonacci_word(int k) {
    assert(k >= 1);
    vector<int> result = {0, 1};
    int a = 1;
    int b = 2;
    for (int i = 1; i < k; i++) {
        assert(result.size() == b);
        result.resize(a + b);
        copy(result.begin(), result.begin() + a, result.begin() + b);
        a = b;
        b = result.size();
    }
    return result;
}


template<typename T>
std::ostream& operator<<(std::ostream &out, const std::vector<T> &v) {
  out << "[";
  bool first = true;
  for (const auto &e : v) {
    if (!first)
      out << ", ";
    first = false;
    out << e;
  }
  out << "]";
  return out;
}
template<typename T1, typename T2>
std::ostream& operator<<(std::ostream &out, const std::pair<T1, T2> &p) {
  out << "(" << p.first << ", " << p.second << ")";
  return out;
}


pair<int, string> traverse(
        const vector<int> &commands,
        vector<int> command0,
        vector<int> command1) {
    set<int> visited;
    int x = 0;
    int prev = adjacent[x].front();

    #define MOVE(angle) { \
        if (visited.count(x)) \
            return {visited.size(), "self-intersection"}; \
        if (x == -1) \
            return {visited.size(), "out of bounds"}; \
        visited.insert(x); \
        int new_x = rotate(adjacent[x], prev, (angle)); \
        prev = x; \
        x = new_x; \
        }

    for (auto c : commands) {
        for (auto angle : c ? command1 : command0)
            MOVE(angle);
    }

    return {visited.size(), "success"};

    #undef MOVE
}


vector<vector<int>> enumerate_sequences(int max_length, vector<int> items) {
    vector<vector<int>> result;
    if (items.empty())
        return result;
    for (int n = 0; n <= max_length; n++) {
        vector<int> cur(n, 0);
        while (true) {
            result.emplace_back();
            for (int x : cur)
                result.back().push_back(items[x]);

            auto p = find_if_not(
                cur.rbegin(), cur.rend(),
                bind1st(equal_to<int>(), items.size() - 1));
            if (p == cur.rend())
                break;
            (*p)++;
            fill(cur.rbegin(), p, 0);
        }
    }
    return result;
}


int main() {
    // Produced by the following command:
    //   ghc DumpGrid.hs -rtsopts -o DumpGrid && ./DumpGrid +RTS -K200000000 -RTS 11 >grid11.txt
    ifstream grid("grid11.txt");

    while (true) {
        int h;
        grid >> h;
        if (grid.eof())
            break;
        assert(h == adjacent.size());
        adjacent.emplace_back();
        for (int& a : adjacent.back())
            grid >> a;
    }
    debug(adjacent.size());


    vector<int> fibonacci_word = build_fibonacci_word(25);

    debug(fibonacci_word.size());

    auto sequences = enumerate_sequences(2, {1, 2, 3, 4, 5, 6});

    for (auto command0 : sequences)
        for (auto command1 : sequences) {
            auto kv = traverse(fibonacci_word, command0, command1);
            if (kv.first > 300) {
                debug2(command0, command1);
                debug(kv);
            }
        }

    return 0;
}
