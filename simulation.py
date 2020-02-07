#!/usr/bin/env python

import csv
import networkx as nx
import EoN


def get_simulation_data():
    N=10**5
    G=nx.barabasi_albert_graph(N, 5) #create a barabasi-albert graph
    tmax = 20
    tau = 0.1           #transmission rate
    gamma = 1.0    #recovery rate
    rho = 0.005      #random fraction initially infected
    sim_inv = EoN.fast_SIR(G, tau, gamma, rho=rho, tmax = tmax, return_full_data=True)
    return sim_inv

def main():
    print "Hello"
    simulation = get_simulation_data()
    with open("transmissions.csv", "w") as output_file:
        writer = csv.DictWriter(output_file, fieldnames=["time", "infector", "infectee"])
        writer.writeheader()
        transmission_dicts = [{"time": t, "infector": u, "infectee": v} for t,u,v in simulation.transmissions()]
        for td in transmission_dicts:
            writer.writerow(td)
    print "Goodbye"

if __name__ == "__main__":
    main()
