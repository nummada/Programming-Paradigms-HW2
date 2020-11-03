command_skel = "swipl -q -t halt -l '{0}' -g '{1}'"

tests_dict = {
    # T1 - tprint
    'eval_tprint' : {
        'tprint_movies' : 'eval(tprint(table(movies)), R)',
        'tprint_ratings' : 'eval(tprint(table(ratings)), R)',
        'tprint_students' : 'eval(tprint(table(students)), R)'
    },

    # T2 - select
    'eval_select' : {
        'select_movies_1' : 'eval(tprint(select(["movie_id"], table(movies))), R).',
        'select_movies_2' : 'eval(tprint(select(["movie_id","title"], table(movies))), R).',
        'select_movies_3' : 'eval(tprint(select(["movie_id","title","genres"], table(movies))), R)',

        'select_ratings_1' : 'eval(tprint(select(["first_name","last_name"], table(ratings))), R).',
        'select_ratings_2' : 'eval(tprint(select(["movie_id","rating"], table(ratings))), R).',
        'select_ratings_3' : 'eval(tprint(select(["last_name","movie_id","rating"], table(ratings))), R).',

        'select_students_1' : 'eval(tprint(select(["first_name","last_name"], table(students))), R).',
        'select_students_2' : 'eval(tprint(select(["first_name","last_name","AA_grade","PP_grade"], table(students))), R).',
        'select_students_3' : 'eval(tprint(select(["first_name","AA_grade","PP_grade","PC_grade"], table(students))), R)',
        'select_students_4' : 'eval(tprint(select(["PP_grade","PA_grade","POO_grade"], table(students))), R).',
    },

    # T3 - join
    'eval_join' : {
        'join_movies_movies' :
            'eval(table(movies),[H|_]), append(H,H,NewCols),                   \
             eval(tprint(join(append, NewCols,                                 \
                            table(movies),                                     \
                            table(movies))), R).',

        'join_students_ratings' :
            'eval(tprint(join(grades_and_ratings,                              \
                            ["AA_grade", "PP_grade", "PC_grade", "PA_grade",   \
                             "POO_grade", "movie_id", "rating"],               \
                            table(students),                                   \
                            table(ratings))), R).',

        'join_movies_ratings' :
            'eval(tprint(join(movies_and_ratings,                              \
                            ["first_name", "last_name", "title",               \
                             "genres", "ratings"],                             \
                            table(movies),                                     \
                            table(ratings))), R).',

        'join_students_movies' :
            'eval(tprint(join(append,                                          \
                            ["first_name", "last_name", "title"],              \
                            select(["first_name", "last_name"],                \
                                    table(students)),                          \
                            select(["title"], table(movies)))), R).',

        'join_students_ratings_2' :
            'eval(tprint(join(check_names,                                     \
                            ["AA_grade", "PP_grade", "PC_grade",               \
                             "PA_grade", "POO_grade", "rating"],               \
                            table(students),                                   \
                            select(["first_name","last_name","rating"],        \
                                    table(ratings)))), R).',
     },

    # T4 - filter
    'eval_tfilter' : {
        'filter_movieid_lt10' :
            'eval(tprint(tfilter([MID,_,_],                                    \
                            (MID < 10),                                        \
                            table(movies))), R).',

        'filter_horror_movies' :
            'eval(tprint(tfilter([_,_,GR],                                     \
                            sub_string(GR,_,_,_,"Horror"),                     \
                            table(movies))), R).',
                 
        'filter_ratings_ht5' :
            'eval(tprint(tfilter([_,_,_,RT],                                   \
                            (RT > 5),                                          \
                            table(ratings))), R).',

        'filter_ratings_bt2and7':
            'eval(tprint(tfilter([_,_,_,RT],                                   \
                            (RT > 2, RT < 7),                                  \
                            table(ratings))), R).',

        'filter_ratings_movieids' :
            'eval(tprint(tfilter([_,_,MID,RT],                                 \
                            (MID > 2, MID < 15, RT >= 3),                      \
                            table(ratings))), R).',

        'filter_students_Blaga' :
            'eval(tprint(tfilter([_,LN|_],                                     \
                            (sub_string(LN, 0, 5, 0, "Blaga")),                \
                            table(students))), R).',

        'filter_students_AA' :
            'eval(tprint(tfilter([AA],                                         \
                            (AA > 5),                                          \
                            select(["AA_grade"], table(students)))), R).',

        'filter_no_dramas_comedies' :
            'eval(tprint(tfilter([_,GR],                                       \
                            (not(sub_string(GR,_,_,_,"Drama")),                \
                             not(sub_string(GR,_,_,_,"Comedy"))),              \
                            select(["title","genres"], table(movies)))), R).',

        'filter_names_ratings' :
            'eval(tprint(tfilter([FN,LN,_,_,RT],                               \
                            names_and_ratings_pred(FN,LN,RT),                  \
                            join(movies_and_ratings,                           \
                                ["first_name", "last_name", "title",           \
                                 "genres", "ratings"],                         \
                                table(movies),                                 \
                                table(ratings)))), R).',

        'filter_y95_movies' :
            'eval(tprint(tfilter([_,_,Title], sub_string(Title,_,_,_,"95"),    \
                            select(["first_name", "last_name", "title"],       \
                                join(append,                                   \
                                    ["first_name", "last_name",                \
                                     "title","genres"],                        \
                                    select(["first_name", "last_name"],        \
                                            table(students)),                  \
                                    select(["title","genres"],                 \
                                            table(movies)))))), R).'
    },
    
    # T5 - complex_query1
    'eval_complex_query1' : {
        'complex_query1_students' :
            'eval(tprint(complex_query1(table(students))), R).',
        
        'complex_query1_students1' :
            'eval(tprint(complex_query1(table(students1))), R).',
        
        'complex_query1_students2' :
            'eval(tprint(complex_query1(table(students2))), R).',
            
        'complex_query1_students3' :
            'eval(tprint(complex_query1(table(students3))), R).'
    },
    
    # T6 - complex_query2
    'eval_complex_query2' : {
        'complex_query2_comedy' :
            'eval(tprint(complex_query2("Comedy", 3, 8)), R).',
        'complex_query2_drama' :
            'eval(tprint(complex_query2("Drama", 6, 9)), _).',
        'complex_query2_crime' :
            'eval(tprint(complex_query2("Crime", 5, 5)), _).',
        'complex_query2_adventure' :
            'eval(tprint(complex_query2("Adventure", 8, 10)), _).'
    }
}

points_dict = {
    'tprint_movies'   : 3,
    'tprint_ratings'  : 3,
    'tprint_students' : 4,
    
    'select_movies_1' : 1,
    'select_movies_2' : 1,
    'select_movies_3' : 1,

    'select_ratings_1' : 1,
    'select_ratings_2' : 1,
    'select_ratings_3' : 1,

    'select_students_1' : 1,
    'select_students_2' : 1,
    'select_students_3' : 1,
    'select_students_4' : 1,
    
    'join_movies_movies'      : 4,
    'join_students_ratings'   : 4,
    'join_movies_ratings'     : 4,
    'join_students_movies'    : 4,
    'join_students_ratings_2' : 4,
    
    'filter_movieid_lt10'       : 2,
    'filter_horror_movies'      : 2,
    'filter_ratings_ht5'        : 2,
    'filter_ratings_bt2and7'    : 2,
    'filter_ratings_movieids'   : 2,
    'filter_students_Blaga'     : 2,
    'filter_students_AA'        : 2,
    'filter_no_dramas_comedies' : 2,
    'filter_names_ratings'      : 2,
    'filter_y95_movies'         : 2,
    
    'complex_query1_students'  : 5,
    'complex_query1_students1' : 5,
    'complex_query1_students2' : 5,
    'complex_query1_students3' : 5,
    
    'complex_query2_comedy'    : 5,
    'complex_query2_drama'     : 5,
    'complex_query2_crime'     : 5,
    'complex_query2_adventure' : 5
}
