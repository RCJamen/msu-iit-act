
###     
###     python3 socket_echo_server.py
###

import socket
import sys
from random import *

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# Bind the socket to the port
server_address = ('localhost', 14344)
print('starting up on {} port {}'.format(*server_address))
sock.bind(server_address)

def help_guide(command):     
    guides = {
        'ADD': 'ADD <N1> <N2> - to add N1 and N2',
        'SUB': 'SUB <N1> <N2> - to subtract N2 from N1',
        'MUL': 'MUL <N1> <N2> - to multiply N1 by N2',
        'DIV': 'DIV <N1> <N2> - to divide N1 by N2',
        'RND': 'RND <N>       - to generate a random number between 1 and N, inclusive',
        'HELP': '''HELP [command] - to display the syntax and semantics of a specific command.
                   If no command is specified, it will display all the available commands and their meanings
                ''',
        'QUIT': 'QUIT - to end the current session of the arithmetic server'
    }

    if len(command) == 1:
        # show all commands
        complete_guide = 'OK The following commands are available:\n'
        for guide in guides:
            complete_guide += '\t' + guides[guide] + '\n'
        return complete_guide

    else:
        try:
            return guides[command[1]]
        except Exception as e:
            return 'ERR {}'.format(e)
        
def get_calculation(command):
    op = command[0]
    ok = 'OK'
    err = 'ERR'

    try:
        if op == 'RND':
            num = int(command[1])
        else:
            num_1 = int(command[1])
            num_2 = int(command[2])

    except Exception as e:
        return err + ' Invalid number of arguments to {}.'.format(op)

    if op == 'ADD':
        return ok + ' {}'.format(num_1+num_2)
    elif op == 'SUB':
        return ok + ' {}'.format(num_1-num_2)
    elif op == 'MUL':
        return ok + ' {}'.format(num_1*num_2)
    elif op == 'DIV':
        try:
            return ok + ' {}'.format(num_1/num_2)
        except Exception as e:
            return err + ' {}.'.format(e)
    elif op == 'RND':
        return ok + ' {}'.format(randint(1, num))
    else:
        return err + ' Unkown operation {}.'.format(op)
    

# Listen for incoming connections
sock.listen(1)

while True:
    # Wait for a connection
    print('waiting for a connection')
    connection, client_address = sock.accept()
    try:
        print('connection from', client_address)
        # Let the client know that you recieved the connection
        connection.sendall('\nThis is CSC 113 Arithmetic Server\nType "HELP" for Guide\n***\n$ '.encode('utf-8'))
        

        # Receive the data in small chunks and retransmit it
        while True:
            data = connection.recv(16)
            decoded_data = data.decode('utf-8').replace('\r\n','')
            command_syntax = decoded_data.split(' ')


            print('Recieved:\n{}'.format(decoded_data))

            if decoded_data: # Check if the message contains a character
                prompt = command_syntax[0]
                if prompt == 'HELP' or 'HELP' in decoded_data:
                    response = help_guide(command_syntax)

                elif 'QUIT' in decoded_data:
                    connection.sendall('OK Bye.\n***\n'.encode('utf-8'))
                    sys.exit()

                elif prompt in ['ADD', 'SUB', 'MUL', 'DIV', 'RND']:
                    response = get_calculation(command_syntax)

                else:
                    response = 'ERR Unknown Operation {}. Type `HELP` to see available commands.'.format(prompt)

                print('Sent:\n{}'.format(response))
                
                msg = '{}\n$ '.format(response)
                encoded_msg = msg.encode('utf-8')
                connection.sendall(encoded_msg)
            else:
                print('no data from', client_address)
                break

    finally:
        # Clean up the connection
        connection.close()