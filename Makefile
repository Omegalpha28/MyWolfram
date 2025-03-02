BINARY_PATH       := $(shell stack path --local-install-root)
BINARY_PATH_BONUS := $(shell cd bonus && stack path --local-install-root)
NAME              = wolfram
NAME_BONUS        = wolfram_bonus
STACK             = stack
STACK_OBJ         = .stack-work

SRC = src/Main.hs \
      src/Rules.hs \
      src/Display.hs \
      src/ErrorHandling.hs \
	  src/Args.hs

BONUS_SRC = bonus/src/Main.hs \
      bonus/src/Rules.hs \
      bonus/src/Display.hs \
      bonus/src/ErrorHandling.hs \
      bonus/src/Cells.hs \
      bonus/src/Secret.hs \
	  bonus/src/Args.hs

all: $(NAME)

install-vty:
	@echo "Waiting for the installation..."
	$(STACK) build --only-dependencies
	$(STACK) install vty
	@echo "Vty installed."

$(NAME): $(SRC)
	$(STACK) build
	cp $(BINARY_PATH)/bin/$(NAME) ./$(NAME)

bonus: fclean install-vty $(BONUS_SRC)
	cd bonus && $(STACK) build --stack-yaml stack.yaml
	cp $(BINARY_PATH_BONUS)/bin/$(NAME_BONUS) ./$(NAME_BONUS)

run: $(NAME)
	$(STACK) exec $(NAME)

run-bonus: $(NAME_BONUS)
	./$(NAME_BONUS)

clean:
	$(STACK) clean
	rm -rf $(STACK_OBJ)

fclean: clean
	rm -f $(NAME) $(NAME_BONUS)

re: fclean all

.PHONY: all clean fclean re run run-bonus

