import requests
import pandas as pd
url = "https://www.unibet.nl/sportsbook-feeds/views/filter/football/netherlands/eredivisie/all/matches?includeParticipants=true&useCombined=true&ncid=1695484561"

headers = {
    # "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/117.0",
    "Content-Type": "application/json",
}
response = requests.get(url, headers=headers)
outputJson = response.json()
print(outputJson)
outputJson = outputJson['layout']['sections'][1]['widgets'][0]['matches']['events']


data_list = []
events = outputJson
event = events[0]

for event in events:
    homeName = event['event']['englishName'].split(" - ")[0]
    awayName = event['event']['englishName'].split(" - ")[1]

    homeOdds = event['betOffers'][0]['outcomes'][0]['odds']/1000
    awayOdds = event['betOffers'][0]['outcomes'][2]['odds']/1000
    drawOdds = event['betOffers'][0]['outcomes'][1]['odds']/1000

    data_list.append([event['event']['start'], event['event']['path'][0]['englishName'], homeName, awayName, homeOdds, awayOdds, drawOdds])
    
# Create a DataFrame
df = pd.DataFrame(data_list, columns=['Game Time(UTC)', 'Game Type Name', 'Team 1', 'Team 2', 'Odds 1', 'Odds 2', 'Draw'])
output_file = f'UBodds.csv'

# Save the DataFrame to a CSV file
df.to_csv(output_file, index=False)