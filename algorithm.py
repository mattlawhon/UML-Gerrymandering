import numpy as np
import networkx as nx
import csv
import time
import copy
import math
import sys


def get_dissim(cluster_ct, G, vap_list, target_districts):

    T = np.sum(vap_list)/target_districts
    dissim = np.zeros((len(vap_list), len(vap_list)))
    min_pos = float('inf')
    max_neg = float('-inf')
    for i in range(cluster_ct):
        for j in range(cluster_ct):
            if(str(i) in G.neighbors(str(j))):
                dissim[i][j] = T - (vap_list[i] + vap_list[j])
                if(vap_list[i] + vap_list[j] >= 1.25*T):
                    dissim[i][j] = 0
                #if(dissim[i][j] < 0):
                #    dissim[i][j] = T/(-1*dissim[i][j]) 
                
                if(dissim[i][j] > 0 and dissim[i][j] < min_pos):
                    min_pos = dissim[i][j]
                if(dissim[i][j] < 0 and dissim[i][j] > max_neg):
                    max_neg = dissim[i][j]
                
                #dissim[j][i] = dissim[i][j]
    numerator = min_pos
    if(numerator == float('inf')):
        numerator = max_neg

    for i in range(cluster_ct):
        for j in range(cluster_ct):
            if(dissim[i][j] < 0):
                dissim[i][j] = -1.0*numerator/dissim[i][j]
                #dissim[j][i] = dissim[i][j]
    

    if(np.sum(dissim) == 0):
        return dissim, True
    
    dissim = dissim/np.sum(dissim)
    return dissim, False


def get_merged_row_col(dissim):
    flattened_dissim = dissim.flatten()
    len_flat_ds = len(flattened_dissim)
    index_arr = range(len_flat_ds)
    val = np.random.choice(index_arr, size=1, p=flattened_dissim)
    row_num, col_num = divmod(val[0], len(dissim))
    return row_num, col_num

def recovery(i,j,data_i,data_j,cluster_dict):
    cluster_dict[i] = data_i
    cluster_dict[j] = data_j
    return cluster_dict


def merge_cluster(n1, n2, G, vap_list, cluster_ct, cluster_dict):
    cluster_ct -= 1
    # n1 is kept, n2 is not kept
    new_G = nx.contracted_edge(G, (str(n1), str(n2)), self_loops=False)
    list_of_nodes = sorted(new_G)
    mapping = {}
    for node in list_of_nodes:
        mapping[node] = node if int(node) < n2 else str(int(node)-1)
    new_G = nx.relabel_nodes(new_G, mapping)

    new_cluster_dict = copy.deepcopy(cluster_dict)
    for val in new_cluster_dict[n2]:
        new_cluster_dict[n1].append(val)
    del new_cluster_dict[n2]

    keys, values = zip(*new_cluster_dict.items())
    keys = list(keys)
    for (i, key) in enumerate(keys):
        keys[i] = key-1 if key >= n2 else key
    new_cluster_dict = dict(zip(keys, values))

    new_vap_list = copy.deepcopy(vap_list)
    new_vap_list[n1] += new_vap_list[n2]
    # new_vap_list[n2] = 0
    new_vap_list = np.delete(new_vap_list, n2)
    return cluster_ct, new_G, new_vap_list, new_cluster_dict


def check_valid(vap_list):
    T = np.sum(vap_list)/target_districts
    for val in vap_list:
        diff = abs(val - T)
        percent = diff / T
        if(percent > 0.25):
            return False
    return True


def recursive_clustering(cluster_dict, cluster_ct, G, vap_list, target_districts, attempt_limit, start_time):

    #base case, checks if final clustering is valid
    if(cluster_ct <= target_districts):
        return cluster_dict, check_valid(vap_list)

    #Retrieves dissimalrity matrix, checks if further clustering is impossible
    dissim, end_early = get_dissim(cluster_ct, G, vap_list, target_districts)
    if(end_early):
        return cluster_dict, False

    #Makes copies to save old values
    old_G = copy.deepcopy(G)
    old_vap_list = copy.deepcopy(vap_list)
    old_cluster_ct = copy.deepcopy(cluster_ct)
    old_cluster_dict = copy.deepcopy(cluster_dict)
    tried_pairs = []

    #Selects one pair to merge and merges them, adds it to tried merges
    row_num, col_num = get_merged_row_col(dissim)
    cluster_ct, G, vap_list, new_cluster_dict = merge_cluster(row_num, col_num, G, vap_list, cluster_ct, cluster_dict)
    attempt_ct = 0
    tried_pairs.append((row_num, col_num))

    flattened_dissim = dissim.flatten()
    possible_choices = [idx for idx, element in enumerate(flattened_dissim) if element != 0]

    while(attempt_ct < attempt_limit):
        curr_time = time.time()
        if curr_time > start_time + 10:
            return new_cluster_dict, False

        #Recursively runs the next pair to merge, if base case is valid clustering, returns true
        new_cluster_dict, val = recursive_clustering(new_cluster_dict, cluster_ct, G, vap_list, target_districts, attempt_limit, start_time)
        if(val):
            return new_cluster_dict, True

        attempt_ct = attempt_ct + 1

        #Since the last attempt led to a failed clustering, let's pick a new merge at the current level
        row_num, col_num = get_merged_row_col(dissim)
        curr_count = 0
        while True:
            if curr_count > len(possible_choices)/2: #no more possible merges at current stage possible
                return old_cluster_dict, False
            if (row_num, col_num) in tried_pairs:
                row_num, col_num = get_merged_row_col(dissim)
                curr_count += 1
            else:
                break
        
        #perform actual merge 
        cluster_ct, G, vap_list, new_cluster_dict = merge_cluster(row_num, col_num, old_G, old_vap_list, old_cluster_ct, old_cluster_dict)

    return new_cluster_dict, False


def write_dict_to_file(sample_list, writer):
    result = []
    for sample in sample_list:
        adj_sample = []
        for (k, v) in sample.items():
            adj_sample.append(v)

        size = 0
        for district in adj_sample:
            size = size + len(district)
        sample_row = np.zeros(size)

        for i in range(len(adj_sample)):
            for j in range(len(adj_sample[i])):
                sample_row[adj_sample[i][j]] = i

        result.append(sample_row)

    result = np.array(result)
    result = result.T
    result = np.matrix(result)
    #print(result)

    for line in result:
        #print(line)
        np.savetxt(writer, line)


# files = ['data/fl25', 'data/fl70', 'data/fl250', 'data/iowa']
# vap for iowa = 7, t_d = 4
files = ['../data/iowa']
total_samples = 10
valid_ct = 0.0
writer = None
sample_list = []
pop_column = 1
target_districts = 4
for file in files:
    if(file == '../data/iowa'):
        pop_column = 3
    if(file == '../data/fl70' or file == '../data/fl250'):
        target_districts = 27

    writer = open(file + "_" + str(total_samples) + '._results_post_update_2.txt', 'w+', encoding='utf-8')
    for sample in range(total_samples):
        G = nx.read_adjlist(file + ".adjlist")

        csvfile = open(file + '.csv', newline='')
        reader = csv.reader(csvfile)

        vap_list = []
        vap_list = np.array(vap_list)
        first = True
        for row in reader:
            if(first):
                first = False
                continue
            vap_list = np.append(vap_list, int(row[pop_column]))

        cluster_ct = len(vap_list)
        orig_len = len(vap_list)
        start_time = time.time()

        cluster_dict = {}
        for i in range(orig_len):
            cluster_dict[i] = [i]

        cluster_dict, val = recursive_clustering(
            cluster_dict, cluster_ct, G, vap_list, target_districts, 10, start_time)
        if(val):
            valid_ct = valid_ct + 1
            sample_list.append(cluster_dict)

        print(str((sample+1) * 100 / total_samples) + " % complete")

write_dict_to_file(sample_list, writer)
writer.close()
print("Success rate: " + str(valid_ct/total_samples*100) + "%")
