#include <iostream>
// #include <assert.h>
using namespace std;

struct link {                           //Making link Structure
     int value;
    link *nextLink;
};

int main(void){

    link *head = new link;              //Initialization
    head -> value = 1;
    link *ptr = head;                   //Reference for making links

    for (int i = 1; i < 20; i++){        //Making links
        ptr -> nextLink = new link;
        ptr -> nextLink -> value = i+1;
        ptr = ptr -> nextLink;
    }

    ptr -> nextLink = head;             //Last "nextLink" will get head
    ptr = head;                         //Initialization for Printing Values

    int counter;
    cout << "Enter number of counts: ";
    cin >> counter;

    for (int i = 0; i < counter; i++){  //Printing
        cout << ptr -> value <<"  ";
        cout << &ptr -> value << endl;
        ptr = ptr -> nextLink;
    }
                                        //Deletion of Memory Adress
    link *tempNode;
    ptr = head;
    for ( int i = 0; i < counter; i++){
        cout << ptr -> value << "x" << endl;
        tempNode = ptr -> nextLink;
        delete ptr;
        ptr = tempNode;
     }
return 0;
}
