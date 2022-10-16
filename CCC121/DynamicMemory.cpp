#include <iostream>
#include <assert.h>

using namespace std;
int main(void){

    int i;
    int **iptntr = new int*[10];
    assert(iptntr != NULL);

    for (i = 0; i < 10; ++i){
        iptntr[i] = new int;
        assert(iptntr != NULL);
    }

    for (i = 0; i < 10; ++i){
        *iptntr[i] = 9 - i;
    }

    for (i = 0; i < 10; ++i){
        cout << *iptntr[i] << " ";
    }
    cout << endl;

    for (i = 0; i < 10; ++i){
        delete iptntr[i];
    }

    delete[] iptntr;

    return 0;
}
