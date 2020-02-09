#!/usr/bin/env python

import csv
import json
import networkx as nx
import EoN


def get_random_network(pop_size):
    """A random network to simulate an epidemic on

    Parameters
    ----------
    params : int
        The number of notes in the graph.

    Returns
    -------
    network : Graph

    """
    network = nx.barabasi_albert_graph(pop_size, 5)
    return network


def get_simulation_data(network, params):
    """The epidemic simulation

    Parameters
    ----------
    network : Graph
        A network to simulate the epidemic on.
    params : dict
        Dictionary of rate parameters.

    Returns
    -------
    sim_inv : Simulation_Investigation

    """
    sim_inv = EoN.fast_SIR(network,
                           params["transmission-rate"],
                           params["recovery-rate"],
                           rho=params["seed-proportion"],
                           tmax=params["tmax"],
                           return_full_data=True)
    return sim_inv


def main():
    print("Hello")
    with open("sim-params.json", "r") as parameter_file:
        params = json.load(parameter_file)
    network = get_random_network(params["population-size"])
    simulation = get_simulation_data(network, params)
    with open(params["output-files"]["transmissions"], "w") as output_file:
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
    print("Goodbye")


if __name__ == "__main__":
    main()
