
// A simple shell in C for the operating systems class.
// Contains some oddities that were part of the spec.


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/wait.h>
#include <time.h>
#include <sys/types.h>



#define BUFSZ 70





void perform_cd(char *input, char *home);
int get_args(char *input, char **progname, char *pargv[]);


int main(int argc, char *argv[]) {
    char input[BUFSZ + 1];
    char *home;
    char *progname;
    char *pargv[36];
    pid_t child;
    int status;
    int i;
    time_t start;


    if ((home = getenv("HOME")) == NULL) {
        printf("Miljövariabeln $HOME är inte definierad! Funktionalitet kommer att saknas.\n");
    }


    while (1) {
        printf("$ ");

        // Read command. If given EOF, exit.
        if (fgets(input, BUFSZ, stdin) == NULL) {
            if (feof(stdin)) {
                putchar('\n');
                break;
            } else {
                printf("Fel vid läsning av stdin. Programmet avslutas.");
                exit(1);
            }
        }

        // Remove the linefeed from the command.
        for (i = 0; i < BUFSZ && input[i] != '\n'; i++);
        if (i < BUFSZ) input[i] = '\0';


        if (strncmp(input, "exit", 5) == 0) {
            // If command is exit, break out of the loop.
            break;
        } else if (strncmp(input, "cd ", 3) == 0) {
            // If command is cd, try to change working directory to the argument to cd.
            perform_cd(input + 3, home);
        } else {
            printf("Externt kommando!\n");
            // TODO: implementera externa kommandon i bakgrunden!
            // Med pollning (wait()) och signalering (skriv signalhanterare)

            get_args(input, &progname, pargv);

            if (progname != NULL) {

                if ((child = fork()) == -1) {
                    printf("Misslyckades med att starta en ny process för kommandot.\n");
                    continue;
                } else if (child == 0) {
                    // execute child
                    if (execvp(progname, pargv) == -1) {
                        printf("Programmet \"%s\" existerar inte eller kunde inte köras.\n", progname);
                        exit(1);
                    }
                }

                start = time(0);
                // WARNING: this cast may lie but printf doesn't
                // support any formatting directive for pid_t
                printf("Skapade förgrundsprocess %i\n", (int) child);

                // Förgrundsprocess
                if (waitpid(child, &status, 0) == -1) {
                    printf("Kunde inte avgöra om kommandot är avslutat. Något är allvarligt fel.\n");
                } else if (WIFEXITED(status) || WIFSIGNALED(status)) {
                    printf("Förgrundsprocess %i avslutades\n", child);
                    printf("Avverkad tid: %.2f\n", difftime(time(0), start));
                }
            }

        }

    }

    printf("Goodbye!\n");

    return 0;
}
                                                                      
void perform_cd(char *arg, char *home) {
    if (chdir(arg) == -1) {
        // If some error occurred...
        switch (errno) {
            // ...and it is because the directory doesn't exist...
            case ENOENT:
                printf("Katalogen finns inte! ");
            // ...or because the path is invalid...
            case ENOTDIR: case EFAULT: case ENAMETOOLONG:
                printf("Ogiltig sökväg.");
                // Change to the home directory if it is known.
                if (home != NULL) {
                    printf(" Byter till hemkatalogen...\n");
                    if (chdir(home) == -1) {
                        printf("Lyckades inte byta till hemkatalogen!\n");
                    }
                } else {
                    putchar('\n');
                }

                break;
            default:
                printf("Allmänt fel vid katalogbyte.\n");
        }
    } else {
        printf("Byter katalog till %s.\n", arg);
    }
}


int get_args(char *input, char **progname, char *pargv[]) {
    char *strtok_saveptr;
    int i;

    *progname = strtok_r(input, " ", &strtok_saveptr);

    pargv[0] = *progname;
    for (i = 1; (pargv[i] = strtok_r(NULL, " ", &strtok_saveptr)) != NULL; i++);

    return 0;
}
 
