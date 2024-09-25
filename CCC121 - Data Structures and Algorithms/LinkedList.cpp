#include <iostream>
#include <assert.h>
using namespace std;

struct Node {
     int value;
    Node* next;
};

int main(void){

    Node* head;
    Node* one;
    Node* two;
    Node* three;

    one = new Node();
    two = new Node();
    three = new Node();

    one->value = 1;
    two->value = 2;
    three->value = 3;

    one->next = two;
    two->next = three;
    three->next = one;

    int counter, i, del;

    cout << "Enter number of counts: ";
    cin >> counter;

    head = one;
    for (i = 0; i < counter; i++){
        cout << head->value <<" ";
        head = head->next;
        assert(head != NULL);
    }
    cout << endl;

return 0;
}