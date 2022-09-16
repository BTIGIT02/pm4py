import pandas as pd

def alignment_to_r(alignment, case_ids):
    list_o_lists = []
    max_len = 0
    for i in range(len(alignment)):
        a_dict = alignment[i]
        case_id = case_ids[i]
        #[case_id, cost, visited_states, queued_states, traversed_arcs, lp_solved, fitness, bwc]
        data_list = [case_id, a_dict["cost"], a_dict["visited_states"], a_dict["queued_states"], 
                     a_dict["traversed_arcs"], a_dict["lp_solved"], a_dict["fitness"], a_dict["bwc"]]

        if len(a_dict["alignment"]) > max_len:
            max_len = len(a_dict["alignment"])
        
        for couple in a_dict["alignment"]:
            data_list.append(couple[0])
        
        list_o_lists.append(data_list)

    headers = ["case_id", "cost", "visited_states", "queued_states", "traversed_arcs", "lp_solved", "fitness", "bwc"]
    headers += [f"...{i+1}" for i in range(max_len)]

    df = pd.DataFrame(columns=headers)
    max_len = len(df.columns)
    i = 0
    for li in list_o_lists:
        if len(li) < max_len:
            diff = max_len - len(li)
            li += [None for i in range(diff)]
        try:    
            df.loc[len(df)] = li
        except ValueError as ve:
            raise(ValueError(f'Error on it-{i}: tried to add row of size {len(li)} to df of size {len(df.columns)}\n{headers}\n{li}'))
        i += 1

    return df
        












