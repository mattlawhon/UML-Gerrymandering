import numpy as np
import networkx as nx
import csv
import time
import copy
from multiprocessing import Pool
import random

N_CPU = 7

random.seed(0)
np.random.seed(0)


def get_dissim(cluster_ct, G, cluster_dict, target_districts, node_indexing):
    vap_list = [cluster_dict[node][1] for node in node_indexing]
    T = np.sum(vap_list) / target_districts
    dissim = np.zeros((len(vap_list), len(vap_list)))
    min_pos = float('inf')
    max_neg = float('-inf')
    for i, node_i in enumerate(node_indexing):
        for j, node_j in enumerate(node_indexing):
            if (node_i in G.neighbors(node_j)):
                dissim[i][j] = T - (vap_list[i] + vap_list[j])
                if (vap_list[i] + vap_list[j] >= 1.25 * T):
                    dissim[i][j] = 0
                # if(dissim[i][j] < 0):
                #    dissim[i][j] = T/(-1*dissim[i][j])

                if (dissim[i][j] > 0 and dissim[i][j] < min_pos):
                    min_pos = dissim[i][j]
                if (dissim[i][j] < 0 and dissim[i][j] > max_neg):
                    max_neg = dissim[i][j]

                # dissim[j][i] = dissim[i][j]
    numerator = min_pos
    if (numerator == float('inf')):
        numerator = max_neg

    for i in range(cluster_ct):
        for j in range(cluster_ct):
            if (dissim[i][j] < 0):
                dissim[i][j] = -1.0 * numerator / dissim[i][j]
                # dissim[j][i] = dissim[i][j]

    if (np.sum(dissim) == 0):
        return dissim, True

    dissim = dissim / np.sum(dissim)
    return dissim, False


def get_merged_row_col(dissim):
    flattened_dissim = dissim.flatten()
    len_flat_ds = len(flattened_dissim)
    index_arr = range(len_flat_ds)
    val = np.random.choice(index_arr, size=1, p=flattened_dissim)
    row_num, col_num = divmod(val[0], len(dissim))
    return row_num, col_num


def recovery(i, j, data_i, data_j, cluster_dict):
    cluster_dict[i] = data_i
    cluster_dict[j] = data_j
    return cluster_dict


def merge(i, j, G, cluster_dict):
    data_i = cluster_dict[i]
    data_j = cluster_dict[j]
    G = nx.contracted_edge(G, (i, j), self_loops=False)
    cluster_dict[i] = (cluster_dict[i][0] + cluster_dict[j][0], cluster_dict[i][1] + cluster_dict[j][1])
    del cluster_dict[j]
    return G, data_i, data_j


def check_valid(cluster_dict, target_districts):
    T = sum(data[1] for data in cluster_dict.values()) / target_districts
    for _, pop in cluster_dict.values():
        diff = abs(pop - T)
        percent = diff / T
        if (percent > 0.25):
            return False
    return True


def check_indexing(g, cluster_dict):
    s1 = set(g.nodes)
    s2 = set(cluster_dict.keys())
    return s1 == s2


def recursive_clustering(cluster_dict, cluster_ct, G, target_districts, attempt_limit, start_time):
    # base case, checks if final clustering is valid
    if (cluster_ct <= target_districts):
        return cluster_dict, check_valid(cluster_dict, target_districts)

    node_indexing = list(G.nodes)

    # Retrieves dissimalrity matrix, checks if further clustering is impossible
    dissim, end_early = get_dissim(cluster_ct, G, cluster_dict, target_districts, node_indexing)
    if (end_early):
        return cluster_dict, False

    tried_pairs = []

    # Selects one pair to merge and merges them, adds it to tried merges
    row_num, col_num = get_merged_row_col(dissim)
    i, j = node_indexing[row_num], node_indexing[col_num]
    attempt_ct = 0
    tried_pairs.append((row_num, col_num))

    flattened_dissim = dissim.flatten()
    possible_choices = [idx for idx, element in enumerate(flattened_dissim) if element != 0]

    while (attempt_ct < attempt_limit):
        curr_time = time.time()
        if curr_time > start_time + 10:
            return cluster_dict, False

        # Recursively runs the next pair to merge, if base case is valid clustering, returns true
        G_new, data_i, data_j = merge(i, j, G, cluster_dict)
        new_cluster_dict, val = recursive_clustering(cluster_dict, cluster_ct - 1, G_new, target_districts,
                                                     attempt_limit, start_time)
        if (val):
            return new_cluster_dict, True

        attempt_ct = attempt_ct + 1
        cluster_dict = recovery(i, j, data_i, data_j, cluster_dict)

        dissim[row_num, col_num] = 0
        if np.sum(dissim) == 0:
            return cluster_dict, False
        dissim = dissim / np.sum(dissim)
        b = check_indexing(G, cluster_dict)

        # Since the last attempt led to a failed clustering, let's pick a new merge at the current level
        row_num, col_num = get_merged_row_col(dissim)
        i, j = node_indexing[row_num], node_indexing[col_num]
    return new_cluster_dict, False


def write_dict_to_file(sample_list, writer):
    result = []
    for sample in sample_list:
        adj_sample = []
        for (k, v) in sample.items():
            adj_sample.append(v[0])

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
    # print(result)

    for line in result:
        # print(line)
        np.savetxt(writer, line)


def sampling(cluster_dict_init, G, target_districts, seed):
    np.random.seed(seed)
    random.seed(seed)
    cluster_dict = copy.deepcopy(cluster_dict_init)
    cluster_ct = len(cluster_dict.keys())
    start_time = time.time()

    cluster_dict, val = recursive_clustering(cluster_dict, cluster_ct, G, target_districts, 10, start_time)
    if not val:
        return None
    return cluster_dict


# files = ['data/fl25', 'data/fl70', 'data/fl250', 'data/iowa']
# vap for iowa = 7, t_d = 4
def main():
    files = ['data/fl25']
    total_samples = 100
    valid_ct = 0.0
    writer = None
    sample_list = []
    pop_column = 1
    target_districts = 4
    for file in files:
        if (file == 'data/iowa'):
            pop_column = 3
        if (file == 'data/fl70' or file == 'data/fl250'):
            target_districts = 27

        writer = open(file + "_" + str(total_samples) + '._results_post_update_2.txt', 'w+', encoding='utf-8')
        G = nx.read_adjlist(file + ".adjlist")

        csvfile = open(file + '.csv', newline='')
        csvfile.readline()
        reader = csv.reader(csvfile)
        cluster_dict_init = {}
        for i, row in enumerate(reader):
            cluster_dict_init[str(i)] = ([i], int(row[pop_column]))

        args = [(cluster_dict_init, G, target_districts, i) for i in range(total_samples)]
        with Pool(N_CPU) as pool:
            sample_list = pool.starmap(sampling, args)

    sample_list = list(filter(lambda x: x is not None, sample_list))
    valid_ct = len(sample_list)
    write_dict_to_file(sample_list, writer)
    writer.close()
    print("Success rate: " + str(valid_ct / total_samples * 100) + "%")


if __name__ == "__main__":
    main()
