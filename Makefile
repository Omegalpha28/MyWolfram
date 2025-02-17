BINARY_PATH		:=	$(shell stack path --local-install-root)
NAME			=	wolfram
STACK			=	stack
STACK_OBJ		=	.stack-work

SRC				=	src/Main.hs \
					src/Rules.hs \
					src/Display.hs \
					src/Rules.hs \
					src/ErrorHandling.hs \
					src/Cells.hs

all: $(NAME)

$(NAME): $(SRC)
	$(STACK) build
	cp $(BINARY_PATH)/bin/$(NAME) ./$(NAME)

run: $(NAME)
	$(STACK) exec $(NAME)

clean:
	$(STACK) clean
	rm -rf $(STACK_OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re run

