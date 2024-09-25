import random

HEADS = "heads"
TAILS = "tails"
MONDAY = "monday"
TUESDAY = "tuesday"
WEDNESDAY = "wednesday"
THURSDAY = "thursday"     
FRIDAY = "friday"

class SleepingBeautyProblem:
    def __init__(self):
        self.monday_wake_ups = 0
        self.tuesday_wake_ups = 0
        self.wednesday_wake_ups = 0
        self.thursday_wake_ups = 0
        self.friday_wake_ups = 0 
        self.total_wake_ups = 0
        self.was_heads_on_wakeup = 0
        self.was_tails_on_wakeup = 0
        self.coin_toss_result_was_heads = 0
        self.coin_toss_result_was_tails = 0
        self.bank = 1000000
        self.gains = 0
        self.loss = 0
        self.bet_wins = 0
        self.bet_lose = 0

    @staticmethod
    def coin_toss():
        if random.randint(1,2) == 1:
            return HEADS
        return TAILS
    
    @staticmethod
    def coin_guess():
        # return HEADS                      #if Halfers
        return TAILS                      #if Thirders        
        # if random.randint(1,2) == 1:      #if random    
        #     return HEADS
        # return TAILS


    def perform_action_on_toss(self, toss_result, guess_result):
        self.bank -= 2
        if toss_result == HEADS:
            self.coin_toss_result_was_heads += 1
            self.perform_heads_action(guess_result)
        else:
            self.coin_toss_result_was_tails += 1
            self.perform_tails_action(guess_result)

    def perform_heads_action(self, guess_result):
        self.wake_up(MONDAY, HEADS, guess_result)
        
    def perform_tails_action(self, guess_result):
        self.wake_up(MONDAY, TAILS, guess_result)
        self.wake_up(TUESDAY, TAILS, guess_result)
        self.wake_up(WEDNESDAY, TAILS, guess_result)
        self.wake_up(THURSDAY, TAILS, guess_result)
        self.wake_up(FRIDAY, TAILS, guess_result)
        
    def wake_up(self, day, toss_result, guess_result):
        self.total_wake_ups += 1
        
        if day == MONDAY:
            self.monday_wake_ups += 1
        else:
            self.tuesday_wake_ups += 1
            self.wednesday_wake_ups += 1
            self.thursday_wake_ups += 1
            self.friday_wake_ups += 1

        if toss_result == HEADS:
            self.was_heads_on_wakeup += 1
        else:
            self.was_tails_on_wakeup += 1

        if toss_result == guess_result:
            self.bet_wins += 1
            self.gains += 3
            self.bank += 3
        else:
            self.bet_lose += 1
            self.loss += 3



    def perform_experiment(self, ammount_of_times):
        for i in range(ammount_of_times):
            toss_result = self.coin_toss()
            guess_result = self.coin_guess()
            self.perform_action_on_toss(toss_result, guess_result)

            if self.bank <= 0:
                return print(f"Insuffecient Money to continue, Total wake up attempts: {self.total_wake_ups}")

        total_money = 1000000

        print(f"Total experiment attempts: {ammount_of_times}")
        print(f"**Thirders Position**\n")
        print(f"Toss result was HEADS: {self.coin_toss_result_was_heads} [{self.coin_toss_result_was_heads / ammount_of_times * 100:.2f}%]")
        print(f"Toss result was TAILS: {self.coin_toss_result_was_tails} [{self.coin_toss_result_was_tails / ammount_of_times * 100:.2f}%]\n ")

        print(f"Toss result was HEADS on wakeup: {self.was_heads_on_wakeup} [{self.was_heads_on_wakeup / self.total_wake_ups * 100:.2f}%]")
        print(f"Toss result was TAILS on wakeup: {self.was_tails_on_wakeup} [{self.was_tails_on_wakeup / self.total_wake_ups * 100:.2f}%]\n")

        print(f"Bet was HEADS on wakeup: [{self.bet_wins / self.total_wake_ups * 100:.2f}%]")
        print(f"Bet was TAILS on wakeup: [{self.bet_lose / self.total_wake_ups * 100:.2f}%]")
        print(f"Total wake ups: {self.total_wake_ups}")
        print(f"Bet Wins: {self.bet_wins}")
        print(f"Bet Lose: {self.bet_lose}\n")
        

        print(f"Total Money: {self.bank}")
        print(f"Gains: {self.bank - total_money}")

def main():
    problem = SleepingBeautyProblem()
    problem.perform_experiment(600000)
    
if __name__ == "__main__":
    main()
