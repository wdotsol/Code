import requests
import json
import pandas as pd
import time
import random
import numpy as np  # Import numpy for NaN

url = "https://server.defibookie.io/api/v1/user/games_by_game_type"

data_list = []

pn = int(input("Enter page number: "))
for x in range(1, pn+1):
    querystring = {"timeline": "0", "page": f"{x}", "keyword": ""}
    headers = {
        "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/117.0",
        "Accept": "application/json",
        "Accept-Language": "en-US,en;q=0.5",
        "Accept-Encoding": "gzip, deflate, br",
        "Referer": "https://defibookie.io/",
        "Authorization": "Bearer undefined",
        "Origin": "https://defibookie.io",
        "Connection": "keep-alive",
        "Sec-Fetch-Dest": "empty",
        "Sec-Fetch-Mode": "cors",
        "Sec-Fetch-Site": "same-site",
        "Content-Length": "0",
        "TE": "trailers"
    }

    r = requests.request("POST", url, headers=headers, params=querystring)
    jsondata = json.loads(r.text)

    for game in jsondata['results']:
        game_type_name = game.get('game_type_name', '')
        game_date = game.get('game_date', '')
        game_time = game_date[11:16] if game_date else ''

        team1 = game.get('team1_name', '')
        team2 = game.get('team2_name', '')

        odds = game.get('odds', {})

        try:
            h2h = odds.get('h2h', {})
            odds1 = round(h2h.get('price1', np.nan), 2)  # Convert to float with decimal point, use np.nan for missing data
            odds2 = round(h2h.get('price2', np.nan), 2)  # Convert to float with decimal point, use np.nan for missing data
            draw = round(h2h.get('draw_price', np.nan), 2)  # Convert to float with decimal point, use np.nan for missing data
        except Exception as e:
            odds1 = np.nan
            odds2 = np.nan
            draw = np.nan

        if draw == 0:
            draw = np.nan

        # Append the data to the list
        data_list.append([game_time, game_type_name, team1, team2, odds1, odds2, draw])
    random_sleep = random.uniform(0.5, 0.9)
    time.sleep(random_sleep)

# Create a DataFrame
df = pd.DataFrame(data_list, columns=['Game Time(UTC)', 'Game Type Name', 'Team 1', 'Team 2', 'Odds 1', 'Odds 2', 'Draw'])
output_file = f'DBodds_{pn}pages.csv'

# Save the DataFrame to a CSV file
df.to_csv(output_file, index=False)
