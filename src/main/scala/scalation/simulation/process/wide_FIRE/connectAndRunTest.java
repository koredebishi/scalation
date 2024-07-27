package scalation.simulation.process.wide_FIRE;

/**
 * This class provides an example of using the DEVS-FIRE API.
 * This example includes multiple steps:
 * Step 1: connect to the DEVS-FIRE API server and obtain a key, which is needed for all following-on calls
 * Step 2: set wind condition for the simulation
 * Step 3: set the location of the fire area using the latitude and longitude info, which decides what fuel and terrain data from the LANDFIRE database will be used
 * Step 4: set ignition point
 * Step 5: start simulation run by giving a simulation time
 *
 * The simulation result of all burning cells' ignition time is returned as a String.
 * To visualize the simulation result, you may use the API_FireState_Visualization class that can be
 * download from https://sims.cs.gsu.edu/sims/research/API_FireState_Visualization.java
 *
 * Copyright: Systems Integrated Modeling and Simulation (SIMS) Lab, Georgia State University, All Rights Reserved
 * Contact: Prof. Xiaolin Hu (xhu@gsu.edu)
 * Date: April 13 2024
 *
 */

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpClient;
import java.net.http.HttpResponse;


public class connectAndRunTest {
    public static void main(String[] args) throws IOException, InterruptedException {
        String base_url = "http://firesim.cs.gsu.edu:8084";
        String simulationResult;

        double windSpeed = 10; // m/s
        double windDirection = 180;  // degree
        double lat = 1526271.822562622;
        double lng = -35619.01701560877;
        int simTime = 10000;
        int ignition_x = 100;
        int ignition_y = 100;

        // Connecting to server
        HttpRequest request1 = HttpRequest.newBuilder()
                .uri(URI.create(base_url+"/api/connect"))
                .header("accept", "application/json")
                .method("POST", HttpRequest.BodyPublishers.ofString("testtest"))
                .build();
        HttpResponse<String> response1 = HttpClient.newHttpClient().send(request1, HttpResponse.BodyHandlers.ofString());
        System.out.println(response1.body());
        String key = response1.body();

        // Setting wind conditions
        HttpRequest request2 = HttpRequest.newBuilder()
                .uri(URI.create(base_url+"/api/setWindCondition/?userToken="+key+"&windSpeed="+windSpeed+"&windDirection="+windDirection))
                .POST(HttpRequest.BodyPublishers.noBody())
                .build();
        HttpResponse<String> response2 = HttpClient.newHttpClient().send(request2, HttpResponse.BodyHandlers.ofString());
        //System.out.println(response2.body());

        //Setting center location
        HttpRequest request3 = HttpRequest.newBuilder()
                .uri(URI.create(base_url+"/api/setCellSpaceLocation/?userToken="+key+"&lat="+lat+"&lng="+lng))
                .POST(HttpRequest.BodyPublishers.noBody())
                .build();
        HttpResponse<String> response3 = HttpClient.newHttpClient().send(request3, HttpResponse.BodyHandlers.ofString());
        //System.out.println(response3.body());

        //Setting ignition Point
        HttpRequest request4 = HttpRequest.newBuilder()
                .uri(URI.create(base_url+"/api/setPointIgnition/?userToken="+key+"&x="+ignition_x+"&y="+ignition_y))
                .POST(HttpRequest.BodyPublishers.noBody())
                .build();
        HttpResponse<String> response4 = HttpClient.newHttpClient().send(request4, HttpResponse.BodyHandlers.ofString());
        //System.out.println(response4.body());

        //running Simulation
        HttpRequest request5 = HttpRequest.newBuilder()
                .uri(URI.create(base_url+"/api/runSimulation/?userToken="+key+"&time="+simTime))
                .POST(HttpRequest.BodyPublishers.noBody())
                .build();
        HttpResponse<String> response5 = HttpClient.newHttpClient().send(request5, HttpResponse.BodyHandlers.ofString());
        simulationResult = response5.body();
        //System.out.println(simulationResult);

        //using the API_FireState_Visualization to visualize the simulation results.
        //you may remove the following two lines of code if you don't need the visualization
        API_FireState_Visualization visual = new API_FireState_Visualization();
        visual.visualize(simulationResult);

        System.out.println("finished!");
    }
}