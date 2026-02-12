#include <stdio.h>

#include "cli_utils/includes/cli_utils.h"

void process(ParsedArgs* pa)
{
    
    printf("Flags:\n");
    while (!Flag_ll_empty(&pa->flag_head))
    {
        Flag* f = Flag_ll_pop(&pa->flag_head);
        printf("\t%s\n", f->name);
    }

    printf("KWArgs:\n");
    while (!KWArg_ll_empty(&pa->kwarg_head))
    {
        KWArg* kwarg = KWArg_ll_pop(&pa->kwarg_head);
        printf("\t%s\n", kwarg->name);
    }

    printf("Args:\n");
    while (!Arg_ll_empty(&pa->arg_head))
    {
        Arg* arg = Arg_ll_pop(&pa->arg_head);
        printf("\t%s\n", arg->name);
    }
}

int main(int argc, char** argv)
{
    App_t* app = new_app(
        "New TP directory maker",
        "new",
        "0.0.1",
        process
    );

    // Flag reverse = {
    //     .name = "reverse",
    //     .symbol = 'r',
    //     .description = "Reverse `text`",
    //     .long_description = "Reverse the text provided\nExamples:\n>>> eko -r coucou\nuocuoc",
    // };
    // push_flag(app, &reverse);

    // KWArg color = {
    //     .name = "color",
    //     .symbol = 'c',
    //     .description = "Print `text` with the given colors",
    //     .long_description = "Print `text` with the given colors.\nAccepted colors: red, green.\nExamples:\n>>> eko -c red coucou\ncoucou",
    //     .value_name = "color"
    // };
    // push_kwarg(app, &color);

    // KWArg output = {
    //     .name = "output",
    //     .symbol = 'o',
    //     .description = "Output where `text` is print out",
    //     .long_description = "Output where `text` is print out\nAccepted outputs: stdout, stderr.\nExamples:\n>>> eko -o stdout coucou\ncoucou",
    //     .value_name = "flux"
    // };
    // push_rkwarg(app, &output);

    Arg name = {
        .name = "name",
        .description = "Name of the new directory",
    };

    push_arg(app, &name);
    
    parse(app, argc, argv);
    
    free_app(app);

    return 0;
}
