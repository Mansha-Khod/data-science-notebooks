import random

word_choice = None

genre = ['Programming languages', 'Animals', 'Countries','Languages']
    
def choose_genre():
    while True:
        choose = input("Enter the genre you want to play: ")
        if choose.isdigit():
            choose = int(choose)
            if 1 <= choose <= 4:
                if choose == 1:
                    return ['python', 'java', 'javascript', 'swift', 'ruby', 'kotlin' ]
                if choose == 2:
                    return ['elephant', 'lion', 'tiger', 'rhino', 'crocodile', 'fish']
                if choose == 3:
                    return ['india', 'australia', 'china', 'japan', 'yemen', 'austria']
                if choose == 4:
                    return ['hindi', 'english', 'german', 'french', 'mandarin', 'japanese']
            else:
                print("Please select numbers from above mentioned genres.")
        else:
            print("Invalid input! Try again!")



def display_win_or_loss(random_word, display_word, attempts):
    if '_' not in display_word:
        print("\nCongratulations! You guessed the word: ",' '.join(display_word))
        print(f"You survived with {attempts} attempts!")
        return True
        
    if attempts == 0:
        print("\nYou lost! The word was:  ",' '.join(random_word))
        print(f"You used all your {attempts} attempts.\nSO, YOU ARE HANGED NOW!")
        return True
    return False
    
def play_game():
    while True:
        print("\nWelcome to hangman game!")
        play = input("Do you want to play (y/n)? ").lower()
        if play == 'n':
            print("Thank you for playing!")
            break
        elif play == 'y':                        
            print("You can select genres from which you'll get you word.")
            print("\nGenres:")

            for i,genres in enumerate(genre, start = 1):
                print(f"{i}. {genres}")
                
            word_choice = choose_genre()
            random_word = random.choice(word_choice)
            display_word = ['_' for _ in random_word]
            attempts = 8
            
            print(f"You have {attempts} attempts to survive.")
            while attempts > 0 and '_' in display_word:
                print(f"\n{' '.join(display_word)}")
                guess = input("Guess a letter: ")
                if guess in random_word:
                    for i, char in enumerate(random_word):
                        if guess == char:
                            display_word[i] = guess
                else:
                    attempts -= 1
                    print(f"Your guess is wrong! You got {attempts} attempts left.")
                if display_win_or_loss(random_word, display_word, attempts):
                    break
        else:
            print("Invalid choice!")
            
play_game()
    