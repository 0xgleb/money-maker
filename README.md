# Money Maker Roadmap

## Init

First we need to build the foundation for the project so that we have something that works but doesn't make money. This includes things like
- Set up the Haskell project
- Set up the Python project
- Integrate Haskell with Python
- Integrate Binance API

I think we can consider this stage done when we can get price data from Binance, pass it to the Python code, and execute a trade on Binance.

## MVP

At this stage we want to build something that has a backtested hard-coded strategy behind it. We want to be able to run the bot and have it make decisions and place trades.
- Set up everything to make backtesting fast and easy
- Create a trading strategy
- Add authentication/authorization
- TBD: Deploy to production

Consider this stage done when you can run the bot and have it make and execute trading decisions.

## Smart Bot

At this point we want to start making the trading strategy smarter by adding machine learning algorithms to it. We also want to build graphs, stats, etc, basically we will need some sort of an interface to understand the performance of the bot. We might want to start supporting multiple strategies at this point, in which case having an interface for managing those will be even more important. To support multiple strategies we will also need to implement more advanced fund management mechanisms.
- Start integrating machine learning algorithms into the trading strategies
- TBD: support multiple strategies within one bot
- Build bot analytics & strategy management interface

I think to consider this stage done, we want to have something running in production and making real-life trades.

## Mission complete

Consider this stage done when we have mansions lol.
