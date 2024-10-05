import requests
import csv




SPORT = 'soccer_usa_mls'

# Bookmaker regions
# uk | us | us2 | eu | au
REGIONS = 'eu'

# Odds markets
# h2h | spreads | totals
MARKETS = 'h2h'

# Odds format
# decimal | american
ODDS_FORMAT = 'decimal'

# Date format
# iso | unix
DATE_FORMAT = 'iso'

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# First get a list of in-season sports
# The sport 'key' from the response can be used to get odds in the next request
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Uncomment the following block to fetch the list of in-season sports
# sports_response = requests.get('https://api.the-odds-api.com/v4/sports', params={
#     'api_key': API_KEY
# })

# if sports_response.status_code != 200:
#     print(f'Failed to get sports: status_code {sports_response.status_code}, response body {sports_response.text}')
# else:
#     print('List of in season sports:', sports_response.json())

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


odds_response = requests.get(f'https://api.the-odds-api.com/v4/sports/{SPORT}/odds', params={
    'api_key': API_KEY,
    'regions': REGIONS,
    'markets': MARKETS,
    'oddsFormat': ODDS_FORMAT,
    'dateFormat': DATE_FORMAT,
})

if odds_response.status_code != 200:
    print(f'Failed to get odds: status_code {odds_response.status_code}, response body {odds_response.text}')
else:
    odds_json = odds_response.json()
    print('Number of events:', len(odds_json))
    print(odds_json)

    # Define the CSV file name
    csv_file = 'odds_data.csv'

    # Open the CSV file for writing
    with open(csv_file, mode='w', newline='') as file:
        writer = csv.writer(file)

        # Write the headers
        writer.writerow(['Game', 'Date', 'Bookmaker', 'Home Team', 'Away Team', 'Draw Odds', 'Home Odds', 'Away Odds'])

        # Write the data
        for event in odds_json:
            game = event['sport_title']
            date = event['commence_time']
            for bookmaker in event['bookmakers']:
                bookmaker_key = bookmaker['key']
                home_team = None
                away_team = None
                home_odds = None
                away_odds = None
                draw_odds = None
                for market in bookmaker['markets']:
                    for outcome in market['outcomes']:
                        if outcome['name'] == event['home_team']:
                            home_team = outcome['name']
                            home_odds = outcome['price']
                        elif outcome['name'] == event['away_team']:
                            away_team = outcome['name']
                            away_odds = outcome['price']
                        elif outcome['name'].lower() == 'draw':
                            draw_odds = outcome['price']
                writer.writerow([game, date, bookmaker_key, home_team, away_team, draw_odds, home_odds, away_odds])

    print(f'Data has been saved to {csv_file}')

    # Check the usage quota
    print('Remaining requests', odds_response.headers['x-requests-remaining'])
    print('Used requests', odds_response.headers['x-requests-used'])
