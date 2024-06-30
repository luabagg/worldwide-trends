# Worldwide Trends

## Project Overview

This project analyzes data collected from the Google Trends public dataset using Google BigQuery. The analysis is performed using OpenAI API, and the results are presented in an interactive map created with JSVectorMap.

## Project Structure

- `main.r`: The script that collects the data from Google BigQuery.
- `classification.r`: The script that performs the data analysis with OpenAI.
- `/out`: Directory containing the output CSV file generated by the R scripts.
- `/site`: App to display the interactive map using JSVectorMap.

## Prerequisites

- R
- Google Cloud account and project
- OpenAI API Key
- Web browser

## Setup and Usage Instructions

1. Clone the repository to your local machine.
2. Ensure you have the necessary prerequisites installed.
3. Setup the .Renviron variables with your values.
4. Execute the following files, in order:
    - `main.r`
    - `classification.r`
    - `merge_terms.r`
5. Run the scripts to perform the data analysis and generate the output CSV file in the `/out` directory.
6. Open `maps/index.html` in a web browser to view the interactive map.

## License

This project is licensed under the MIT License.
