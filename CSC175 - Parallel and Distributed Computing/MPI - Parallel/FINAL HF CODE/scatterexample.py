from mpi4py import MPI

# Get the MPI rank and size
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

def chunk_file(file_path, chunk_size):
    # Open the file
    with open(file_path, 'r') as file:
        # Read the file contents
        file_data = file.read()

        # Calculate the total number of characters in the file
        file_size = len(file_data)

        # Calculate the number of chunks
        num_chunks = (file_size + chunk_size - 1) // chunk_size

        # Calculate the chunk range for each process
        chunk_start = rank * (num_chunks // size)
        chunk_end = (rank + 1) * (num_chunks // size)
        if rank == size - 1:
            chunk_end = num_chunks

        # Get the starting and ending indices of the chunk
        start_index = chunk_start * chunk_size
        end_index = min((chunk_end * chunk_size), file_size)

        # Extract the chunk of data
        chunk_data = file_data[start_index:end_index]

        # Process the chunk (e.g., analyze, manipulate, etc.)
        process_chunk(chunk_data, chunk_start)

def process_chunk(chunk_data, chunk_index):
    class NodeTree(object):
        def __init__(self, left=None, right=None):
            self.left = left
            self.right = right

        def children(self):
            return (self.left, self.right)

        def nodes(self):
            return (self.left, self.right)

        def __str__(self):
            return '%s_%s' % (self.left, self.right)


    # Main function implementing huffman coding
    def huffman_code_tree(node, left=True, binString=''):
        if type(node) is str:
            return {node: binString}
        (l, r) = node.children()
        d = dict()
        d.update(huffman_code_tree(l, True, binString + '0'))
        d.update(huffman_code_tree(r, False, binString + '1'))
        return d

    # Calculating frequency
    freq = {}
    for c in chunk_data:
        if c in freq:
            freq[c] += 1
        else:
            freq[c] = 1

    # Gather frequencies from all processes
    freq = comm.gather(freq, root=0)

    # Process 0 combines frequencies and performs huffman coding
    if rank == 0:
        # Merge frequencies from all processes
        merged_freq = {}
        for f in freq:
            for key, value in f.items():
                if key in merged_freq:
                    merged_freq[key] += value
                else:
                    merged_freq[key] = value
    else:
         merged_freq = {}


    # Sort the frequencies
    freq_items = sorted(merged_freq.items(), key=lambda x: x[1], reverse=True)

    if freq_items:
        nodes = freq_items

        while len(nodes) > 1:
            (key1, c1) = nodes[-1]
            (key2, c2) = nodes[-2]
            nodes = nodes[:-2]
            node = NodeTree(key1, key2)
            nodes.append((node, c1 + c2))

            nodes = sorted(nodes, key=lambda x: x[1], reverse=True)

        huffmanCode = huffman_code_tree(nodes[0][0])

        print(' Char | Huffman code ')
        print('----------------------')
        for (char, frequency) in freq_items:
            print(' %-4r |%12s' % (char, huffmanCode[char]))
    else:
        print("No frequencies found. Exiting.")

if __name__ == "__main__":
    
    # Specify the file path and chunk size
    file_path = "textfile.txt"
    chunk_size = size  # Adjust the chunk size as per your requirements

    # Call the chunk_file function
    chunk_file(file_path, chunk_size)

