#!/usr/bin/env python

import csv
import json
import networkx as nx
import EoN


def get_simulation_data(params):
    network = nx.barabasi_albert_graph(params["population-size"], 5)
    sim_inv = EoN.fast_SIR(network,
                           params["transmission-rate"],
                           params["recovery-rate"],
                           rho=params["seed-proportion"],
                           tmax=params["tmax"],
                           return_full_data=True)
    return sim_inv


def main():
    print "Hello"
    with open("sim-params.json", "r") as parameter_file:
        params = json.load(parameter_file)
    simulation = get_simulation_data(params)
    with open("transmissions.csv", "w") as output_file:
        writer = csv.DictWriter(output_file,
                                fieldnames=["time", "infector", "infectee"])
        writer.writeheader()
        transmission_dicts = [{
            "time": t,
            "infector": u,
            "infectee": v
        } for t, u, v in simulation.transmissions()]
        for td in transmission_dicts:
            writer.writerow(td)
    print "Goodbye"


if __name__ == "__main__":
    main()
