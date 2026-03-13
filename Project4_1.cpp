# include <iostream>
# include <string>
#include <unordered_set>
#include <math.h>
#include <vector>
#include <unordered_map>
#include <iomanip>
#include <algorithm>
#include <map>
#include <memory>
using namespace std;
// <S-exp> ::= <ATOM> 
//             | LEFT-PAREN <S-exp> { <S-exp> } [ DOT <S-exp> ] RIGHT-PAREN
//             | QUOTE <S-exp>
            
// <ATOM>  ::= SYMBOL | INT | FLOAT | STRING 
//             | NIL | T | LEFT-PAREN RIGHT-PAREN
enum TokenType { Procedure ,SYMBOL, INT, FLOAT, STRING, NIL, T, LEFT_PAREN, RIGHT_PAREN, ERROR, Space};  // TokenType
enum StructureType { ATOM, QUOTE, CONSTANT, S_EXP, NONE };  // StuctureType
enum ErrorType { No_Error, UNEXPECTED_DOT, UNEXPECTED_RIGHT_PAREN, UNEXPECTED_EOF, UNEXPECTED_STRING, UNEXPECTED_TOKEN };  // ErrorType
enum FormatError { FORMAT_CORRECT, LEVEL_EXIT ,LEVEL_CLEAN ,UNBOUND_SYMBOL, UNEXPECTED_TYPE, 
    UNEXPECTED_FORMAT, UNEXPECTED_ARGS_NUMBER, ATTEMPT_NON_FUNCTION, DIVISION_ZERO, 
    NON_RETURN_VALUE, COND_ERROR, NON_LIST, LEVEL_DEFINE, LAMBDA_ERROR, 
    UNBOUND_PARAMETER, UNBOUND_COND, UNBOUND_TEST_COND, LET_FORMAT, NON_COND, NON_Value_Variable,
    SET_FORMAT };  // FormatError
bool NextEOF = false; // The flag of the EOF
struct ASTNode { // The structure of the AST node
    string token;
    TokenType type;
    StructureType st;
    shared_ptr<ASTNode> left = nullptr;
    shared_ptr<ASTNode> right = nullptr;
    int level;
    ASTNode() {
        token = " ";
        type = Space;
    } // 空的node

    ASTNode( TokenType t, StructureType s, string tok, int lev ) { // The constructor of the ASTNode class
        type = t;
        st = s;
        token = tok;
        level = lev;
    }
    void Insert( shared_ptr<ASTNode> node, StructureType st, int lev ) { // Insert a node into the AST tree according to the type of the token
        if (left == nullptr) {left = node;}
        else if (right == nullptr) {
            right = make_shared<ASTNode>(Space,NONE,"", lev);
            right->Insert(node, st, lev);
        }
        else {
            right->Insert(node, st, lev);
        }
    }
};  // ASTNode
class Scanner { // The class of the Scanner (Project 1)
    public:
        TokenType type; // The type of the token
        int line = 1; // 目前處裡到哪列
        int column = 0; // 目前處裡到哪行
    
        shared_ptr<ASTNode> root; // The root of the AST tree
        ErrorType error; // The type of the error
        Scanner() { // The constructor of the Parser class
            root = nullptr;
            type = SYMBOL;
            error = No_Error;
        }
        // Read the character from the input
        int PeekChar() {
            int p = cin.peek();
            if (p == EOF) {
                NextEOF = true;
                return -1;
            }
            return cin.peek();
        }
        bool inputChar(char &ch) {
            int p = PeekChar();
            if ( p == -1 || NextEOF ) { // EOF
                return false;
            }

            if ( cin.get(ch) ) {
                if ( ch == '\n' ) { line++; column = 0; }

                else column++;
                return true;
            }

            if ( cin.eof() ) { error = UNEXPECTED_EOF; return false; }

            return false;
        }
        // Clean the line where the error occurs
        void ErrorCleanLiner( ErrorType et ) { 
            if (et == UNEXPECTED_STRING) { return; }
            string garbage;
            error = et;
            getline(cin, garbage, '\n');
        }
        // Check if the program should exit
        void CheckExit(string &expr) {
            if (root) {   
                if ( root -> token == "nil" ) {
                    return;
                }
                else if (root -> left -> token == "exit" && !root -> right ) 
                { expr = "(exit)";} // Exit the program
            }
        }
        // Determine whether should but into the AST tree
        string ReadSExp( ) { 
            string expr = "";
            char ch; // The character read from the input
            shared_ptr<ASTNode> current;
            int p = cin.peek();
            if (p == EOF) { 
                error = UNEXPECTED_EOF; return expr; }

            while ( inputChar(ch) ) { // Read the S-expression
                p = cin.peek();
                // Skip the white space
                if (ch == ' ' || ch == '\t' || ch == '\n') { continue; } 
                // S-exp: LEFT_PAREN S-exp RIGHT_PAREN              
                if (ch == '(' ) { 
                    if ( root == nullptr ) {
                        root = make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", 0);
                        expr = ReadSExp(root, ch);
                    }

                    else { // '(
                        current -> left = make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", current->level+1);
                        expr = ReadSExp( current -> left, ch);
                    }
                    
                    if (error != No_Error) { ErrorCleanLiner(error);return expr; }
                    CheckExit(expr);
                    break;
                }
                // S-exp: Quote Structure
                else if (ch == '\'') { 
                    GOTQuote( root, ch, expr ); // 進入Quote <S-exp>
                    if (error != No_Error) { ErrorCleanLiner(error); return expr; }
                    expr.clear();
                    break;
                }
                // line-comment
                else if ( ch == ';') { CleanLineComment(); continue;}
                // Float or Symbol, NOT DOT!!!
                else if (ch == '.') {
                    expr = ch;
                    if ( p == ' ' || p == '\t' || p == '\n' || p == '(' || p == ')' ) { ErrorCleanLiner(UNEXPECTED_DOT); return expr; } // . Error

                    else if (isdigit(p)) { // FLOAT
                        inputChar(ch);
                        GetToken(expr, ch);
                        if (isFloat(expr)) { type = FLOAT; }
                        else { type = SYMBOL;}

                        root = make_shared<ASTNode>( type, ATOM, expr, 0 );
                        break;
                    }

                    else { // Symbol start with DOT
                        inputChar(ch);
                        type = ReadAtom(ch, expr);
                        root = make_shared<ASTNode>( type, ATOM, expr, 0 );
                        break;
                    }
                }
                // Error of Right Parenthesis
                else if ( ch == ')' ) { ErrorCleanLiner( UNEXPECTED_RIGHT_PAREN ); return expr; }
                // STRING
                else if ( ch == '\"' ) { 
                    expr = ch;
                    if (!isString(expr)) { ErrorCleanLiner(UNEXPECTED_STRING); return expr;}
                    else {
                        root = make_shared<ASTNode>( STRING, ATOM, expr, 0 );
                    }
                    break;
                }
                // Other kinds of Atoms
                else {
                    type = ReadAtom(ch, expr);
                    if (type == NIL) { expr = "nil";}
                    else if (type == T) { expr = "#t"; }
                    root = make_shared<ASTNode>( type, ATOM, expr, 0 );
                    break;
                }     
                // EOF
                if ( p == EOF ) { 
                    error = UNEXPECTED_EOF; return expr; }
            } // while
            if ( ch != '\n' && ch != ';' ) 
                LineCleanSpace();
            if ( NextEOF && root == nullptr ) { // EOF
                error = UNEXPECTED_EOF; return expr; 
            }
            return expr;
        } // ReadSExp
        // Read the S-expression and put it into a AST tree
        string ReadSExp( shared_ptr<ASTNode>current, char ch ) {       
            bool usedDOT = false;
            string expr = "";
            // 首先進來一定是root有一個左括號節點
            // 然後開始讀取下一個字元
            // 如果是)合併成nil，將此SEXP括號替換成ATOM nil的node
            // 如果是'則進入GOTQuote
            // 如果是.則進入GotDOT
            // 如果是;則進入CleanLineComment
            // 其餘進入InsertAtom 
            while( error == No_Error && inputChar(ch) && !cin.eof() ) { // Read the S-expression
                if (ch == '\n' || ch == ' ' || ch == '\t' ) { continue; }
                
                else { // Token
                    if ( usedDOT && ch != ')' && ch != ';') { 
                        if ( ch == ';' ) {
                            CleanLineComment();
                            continue;
                        }

                        error = UNEXPECTED_TOKEN; 
                        expr.clear();
                        if ( ch == '(' )
                            return "(";
                        GetToken(expr, ch);
                        return expr; 
                    }

                    if ( ch == ')' ) { // The end of the S-expression or the end of the list
                        if ( !current -> left && current -> token == "(" ) { // 現在的current是左括號
                            current -> token = "nil";
                            current -> type = NIL;
                            current -> st = ATOM;
                        }

                        return expr;
                    }

                    if (ch == '(' ) { // S-exp2
                        // 新增一個新的Left_Paren node
                        current->Insert(make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", current->level+1), ATOM, current->level+1);
                        if ( current -> right ) { current = current->right; }
                        expr = ReadSExp(current->left, ch); // 進入下一層
                        if ( current -> right ) { current = current->right; }
                    }

                    else if (ch == '\'') { // S-exp3
                        GOTQuote( current, ch, expr );
                        if ( error != No_Error ) { return expr; }
                        if ( current -> right ) { current = current->right; }
                        expr.clear();
                    } // else if (ch == '\'') Quote

                    else if (ch =='.') { // S-exp2 ( DOT ) float
                        // 先確保有左NODE，DOT才成立
                        GotDOT( current, ch, usedDOT, expr );
                        if ( error != No_Error ) { return expr;}
                        if ( current -> right ) { current = current->right;}
                        expr.clear();
                        if ( ch == ')' ) return expr; 
                    }
                    
                    else if (ch == ';') { // line-comment
                        CleanLineComment();
                    }

                    else { // Atom
                        expr = InsertAtom( current, ch, "");
                        if ( current -> right ) { current = current->right; }
                        if ( error != No_Error ) { return expr; }
                        expr.clear();
                        if ( ch == ')' ) return expr; // break;
                    } 
                }
            } // Read the S-expression
            if ( error != No_Error ) { return expr; }
            if ( NextEOF ) { // EOF
                error = UNEXPECTED_EOF; return expr; 
            }
            return expr;
        } // ReadSExp
        // Insert an Atom into the AST tree
        string InsertAtom( shared_ptr<ASTNode> current, char &ch, string expr ) { 
            type = ReadAtom(ch, expr);
            string garbage;
            switch (type) {
                case NIL:
                    expr = "nil";
                    break;
                case T:
                    expr = "#t";
                    break;
                case ERROR:
                    return "Error";
                default:
                    break;
            }
            // Insert New Node Atom to AST
            current->Insert(make_shared<ASTNode>( type, ATOM, expr, current->level ), ATOM, current->level);
            expr.clear();
            return expr;
        }
        // current 初始會在上層之Space節點上
        void GotDOT( shared_ptr<ASTNode> current ,char &ch, bool &used, string &expr ) {       
            expr = ".";
            
            // Check whether is Float or DOT
            int p = cin.peek(); 
            // FLOAT ATOM      
            if (isdigit(p)) {
                inputChar(ch);
                expr = "."; // DOT
                expr = InsertAtom(current, ch, expr);
                if ( current -> right ) { current = current->right; }
                expr.clear();  
                return;
            }

            // SYMBOL
            else if ( p != '(' && p != ')' && p != '\n' && p != '\t' && p != ';' && p != ' ' && p != '\"') { 
                inputChar(ch);
                expr = InsertAtom( current, ch, expr);
                if ( current -> right ) { current = current->right; }
                expr.clear();
                return;
            }

            if ( used ) {error = UNEXPECTED_DOT; return;} // Only use one time DOT
            if ( current -> left == nullptr ) { error = UNEXPECTED_DOT; return ; }
            CleanSpace(ch);
            used = true;
            if ( ch == ')' ) { // .)
                error =  UNEXPECTED_RIGHT_PAREN;
                return;
            }

            // DOT Structure
            expr.clear();
            if ( ch == '(' ) { // Dot pair with S-exp .(
                expr = ReadSExp(current, ch); 
                return;
            }

            else if ( ch == ';' ) { // Line comment
                CleanLineComment();
                used = false;
                ch = '.';
                expr = ".";
                GotDOT( current, ch, used, expr ); 
                return;
            }

            else if ( ch == '\'') { // Dot pair with Quote .'
                shared_ptr<ASTNode> temp = make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", current->level+1);
                GOTQuote( temp , ch, expr );
                if ( error != No_Error ) { return;}
                current->right = temp -> left;
                current->right->st = NONE;
                current->right->type = Space;
                current->right->token = "";
                temp->left = nullptr;
                Delete(temp);
                return;
            }

            else if ( ch == '.' ) { // ..
                GotDOT( current, ch, used, expr );
                return;
            }

            else { // Dot pair with Atom. Insert Dot structure to AST // (X . X)
                type = ReadAtom(ch, expr);
                if ( type != NIL ) { 
                    if ( type == T ) { expr = "#t"; }
                    current -> right = make_shared<ASTNode>( type, ATOM, expr, current->level );
                }
                expr.clear(); 
            }
        }
        // current 初始會在上層之Space節點上
        // temp新增左括號，quote，右空白之節點
        // current->Insert(temp) 新增上層與本層之連接節點 
        // current = temp -> right 進入下一層
        // Return 回去: Previous-有error回到上層節點，current-正常回到本層目前節點
        void GOTQuote( shared_ptr<ASTNode> Previous, char &ch, string &expr ) { // Quote with S-exp
            if ( error != No_Error ) { return; }
            int level = 0;
            if ( Previous != nullptr ) { level = Previous->level + 1; }
            shared_ptr<ASTNode> current = nullptr; // 這層的進度 
            shared_ptr<ASTNode> temp = make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", level);
            temp -> left = make_shared<ASTNode>( SYMBOL, ATOM, "quote", level);
            temp -> right = make_shared<ASTNode>( Space, NONE, " ", level);
            if ( Previous == nullptr ) {root = temp; } // 如果current是root 
            else { Previous -> Insert(temp, ATOM, level);}
            current = temp -> right;
            CleanSpace(ch);
            if (ch == '(') { // Quote with S-exp
                current->left = make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", current->level+1);
                expr = ReadSExp(current->left, ch); // 進入下一層
            }
            else if (ch == '.') { // Quote with DOT '.12, but no '.
                int Dot_p = cin.peek();
                if ( Dot_p == ' ' || Dot_p == '\t' || Dot_p == '\n' || Dot_p == '(' || Dot_p == ')' ) { // . Error
                    error = UNEXPECTED_DOT;
                    return;
                }
                // Float or Symbol
                inputChar(ch);
                expr = ".";
                expr = InsertAtom( current, ch, expr );
                if ( current -> right ) { current = current->right; }
                expr.clear();
                if ( ch == ')' ) return;
            }

            // ') Error
            else if (ch == ')') { 
                error =  UNEXPECTED_RIGHT_PAREN;
            }
            // Quote with Quote
            else if (ch == '\'') { 
                GOTQuote( current, ch, expr );
            }
            // Atom
            else { 
                expr = InsertAtom( current, ch, expr );
                expr.clear();
                if ( current -> right ) { current = current->right; }
                if ( ch == ')' )  PutBack( vector<char>{')'});
            }

            return;
        }
 
        void CleanLineComment() {
            int p = cin.peek();
            if ( p == EOF ) { return ;}
            else {
                char ch;
                string comment;
                if ( p == 10 ) cin.get(ch);
                else getline(cin, comment, '\n');
                line++;
                column = 0;
            }
        } // Clean the line comment
        // Read the Atom
        TokenType ReadAtom( char &c, string& atom ) {
            // <ATOM>  ::= SYMBOL | INT | FLOAT | STRING 
            //             | NIL | T | LEFT-PAREN RIGHT-PAREN
            if ( c == '\"' ) {   // STRING
                atom.push_back(c);
                if (isString(atom)) { return STRING; }
                else if ( error == UNEXPECTED_STRING ) {return ERROR;}
            }

            else {
                GetToken(atom, c);
                // NIL
                if (atom == "nil" || atom == "#f" || atom == "()" ) { return NIL; }
                // T
                else if ( atom == "t" || atom == "#t" )  { return T; }
                // INT
                else if ( isInt(atom) ) {
                    if ( atom[0] == '+' ) { atom.erase(atom.begin()); }
                    return INT;
                }
                // FLOAT Handle the float
                else if (isFloat(atom)) return FLOAT;
                // SYMBOL
                else return SYMBOL; 
            }
            return ERROR;
        } // ReadAtom
        // Clean the space and get next token's head
        void CleanSpace( char &c ) {
            while ( inputChar(c) ) { // Read the Atom
                if ( c == ';' )  { // Skip the white space
                    CleanLineComment();
                }
                else if (c != ' '&& c != '\t' && c != '\n') // Skip the white space
                    return;
            }
        }
        // Get the token
        void GetToken(string &atom, char &c) {   
            atom.push_back(c);
            while ( inputChar(c) ) {
                // Get this Separator must put it back
                // if ( cin.eof() ) { // EOF
                //     cin.putback(c);
                //     cout<<"Encoutering EOF"<<endl;
                // }

                if ( c == '(' || c == '\"' || c == '\'' || c == ' ' || c == '\t' || c == ')') {
                    cin.putback(c);
                    column--;
                    c = '\0';
                    break;
                }

                else if ( c == '\n' ) {
                    break;
                }

                if ( c == ';' ) {
                    CleanLineComment();
                    break;
                }

                atom += c;
            }
            return;
        }
        // Put back the character
        void PutBack( vector<char> temp ) { 
            for (int i = temp.size()-1; i >= 0; i--)
                cin.putback(temp[i]);
        }

        // Line Cleaner for Space.
        // Peeking next character
        // if it is not space or tab or enter
        // put it back.
        void LineCleanSpace( ) {
            char ch;
            int peek = cin.peek();
            if ( peek == EOF || peek == '\0' ) { return; }
            if ( peek == '\n' ) { inputChar(ch); return; } // 遇到下一個是換行符號，把他吃掉後直接return
            vector<char> temp;
            while ( cin.get(ch) && ch != '\n' ) {
                if ( ch == ';' ){
                    CleanLineComment();
                    break;
                }

                temp.push_back(ch);
                if ( ch != ' ' && ch != '\t' ) {
                    PutBack(temp);
                    return;
                }
            }
        }

        bool isInt(string str) { // Check if the string is an integer
            int start = 0;
            if ( str.length() == 1 && !isdigit(str[0]) ) return false;
            if ( str[0] == '+' || str[0] == '-' ) start = 1;

            for (int i = start; i < str.length(); i++) {
                if ( !isdigit(str[i]) ) return false;
            }

            return true;
        }

        bool isFloat(string &str) { // Check if the string is a float
            bool dot = false;
            int start = 0;
            if (str[0] == '+' || str[0] == '-') {
                if (str.length() == 1) return false; // 單獨的 "+" 或 "-" 不是數字
                start = 1;
            }

            for (int i = start; i < str.length(); i++) { // Only check is float
                if ( !isdigit(str[i]) && str[i] != '.' ) return false;

                if (str[i] == '.')
                {
                    if (dot) return false;
                    dot = true;
                }
            }

            if (str == "." || str == "+." || str == "-.") return false;

                // 若以小數點開頭，例如 ".123"，自動補 0 變成 "0.123"
            if (str[0] == '.') str = "0" + str;
            else if (str.length() > 1 && str[0] == '-' && str[1] == '.') str.insert(1, "0"); // -.2 -> -0.2
            else if (str.length() > 1 && str[0] == '+' && str[1] == '.'){ str.erase(0, 1); str.insert(0, "0");} // +.2 -> 0.2
            else if ( str.length() > 1 && str[0] == '+' ) { str.erase(0, 1); } // +1.23 -> 1.23
            return true;
        }

        bool isString(string &str) { // Check if the string is a string
            char ch;
            bool skew = false;
            bool quote = false;
            int temp_column = column;
            unordered_set<char> symbol = { '\"' ,'n', 't', '\''};
            while (inputChar(ch) && ch != '\n') { // Read the Atom
                temp_column = column;
                if ( ch == '\"' && !skew) { // 結束字串
                    str.push_back(ch);
                    return true;
                }

                if ( ch == '\\' ) {
                    if (skew) str.pop_back();
                    skew = !skew;                  
                }

                else if ( symbol.find(ch) != symbol.end() ) { // 處裡 '\(特殊符號)' 的情況
                    if (skew) { // '\n' '\t'
                        str.pop_back();
                        skew = false;
                        switch (ch) {
                            case 'n':
                                str.push_back('\n');
                                break;
                            case 't':
                                str.push_back('\t');
                                break;
                            case '\'':
                                str.push_back('\'');
                                break;
                            case '\"':
                                str.push_back('\"');
                                break;
                            default:
                                continue;
                        }
                        continue;
                    }
                } // if ( symbol.find(ch) != symbol.end() )
                else skew = false;
                
                str.push_back(ch);
            } // while 


            if ( str[0] == '\"' && str[str.length()-1] != '\"' || ch == '\n' ) { // 結束字串
                if ( !NextEOF ) { // EOF
                    line--;
                }
                error = UNEXPECTED_STRING;
                column = temp_column + 1;
            }

            return false;
        }

        bool isdigit( char ch ) { return (ch >= '0' && ch <= '9'); }

        void PrintErrorMessage(string expr) { // Pretty Print the S-expression
            if (error == UNEXPECTED_DOT) {
                cout<<"ERROR (unexpected token) : atom or '(' expected when token at Line "<<line<<" Column "<<column<<" is >>.<<"<<endl;
                return;
            }

            else if (error == UNEXPECTED_RIGHT_PAREN) {
                cout<<"ERROR (unexpected token) : atom or '(' expected when token at Line "<<line<<" Column "<<column<<" is >>)<<"<<endl;
                return;
            }

            else if (error == UNEXPECTED_EOF){
                cout<<"ERROR (no more input) : END-OF-FILE encountered"<<endl;
                return;
            }

            else if (error == UNEXPECTED_STRING) {
                cout<<"ERROR (no closing quote) : END-OF-LINE encountered at Line "<<line<<" Column "<<column<<endl;
                return;
            }

            else if ( error == UNEXPECTED_TOKEN ){
                cout<<"ERROR (unexpected token) : ')' expected when token at Line "<<line<<" Column "<<column<<" is >>"<<expr<<"<<"<<endl;
                return;
            } 
        }
        
        void PrintSExp( shared_ptr<ASTNode> current, int level ) { // Print the AST tree
            if ( current == nullptr ) { return; }
            if ( current->type != LEFT_PAREN && current->type != Space ) { // 只有一個node的情況
                if ( current ->st == CONSTANT ) {
                    cout<<"#<procedure "<<current->token<<">"<<endl;
                    return;
                }

                else {
                    if ( current->type == FLOAT ) {
                        string temp = current->token;
                        cout<< fixed << setprecision(3)<<stod(temp)<<endl;
                    }

                    else
                        cout<<current->token<<endl;
                    return;
                }
            }

            else {
                cout<<"( ";
                if (current->left-> type != LEFT_PAREN && current->left->type != Space   ) { // Print ATOM
                    if ( current->left->st == CONSTANT ) {
                        cout<<"#<procedure "<<current->left->token<<">"<<endl;
                    }                    
                    else { // Print ATOM ( or ) QUOTE
                        if ( current->left->type == FLOAT ) {
                            string temp = current->left->token;
                            cout << fixed << setprecision(3)<<stod(temp)<<endl;
                        }

                        else {
                            cout<<current->left->token<<endl; 
                        }
                    }   
                }

                else { // Go to next S-exp
                    PrintSExp(current->left, level+1);
                }
                
                if ( current->right && (current -> right-> type != LEFT_PAREN && current->right->type != Space) ) { // Print ATOM ( dotted pair )
                    PrintSpace(level);
                    cout<<"  ."<<endl;
                    PrintSpace(level);
                    if (current->right->st == CONSTANT)
                        cout<<"  #<procedure "<<current->right->token<<">"<<endl;
                    else {
                        if ( current->right->type == FLOAT ) {
                            string temp = current->right->token;
                            cout <<"  "<<fixed << setprecision(3)<<stod(temp)<<endl;
                        }

                        else cout<<"  "<<current->right->token<<endl;
                    }

                }

                else 
                    PrintRight( current, level );

                PrintSpace(level);
                cout<<")"<<endl;
                return;
            }
        }
        
        void PrintRight( shared_ptr<ASTNode>current, int level ) {
            while (current->right) {
                current = current->right;
                if ( current -> type == Space ) { // 右邊節點print空白
                    if ( current ->left-> type != LEFT_PAREN && current->left->type != Space ) { // Print ATOM or Quote
                        PrintSpace(level);
                        if ( current->left->st == CONSTANT ) { // Print primptive
                            cout<<"  #<procedure "<<current->left->token<<">"<<endl;
                        }
                        else { // Print ATOM ( or ) QUOTE
                            if (current->left->type == FLOAT) {
                                string temp = current->left->token;
                                cout<<"  " << fixed << setprecision(3)<<stod(temp)<<endl;
                            }

                            else
                                cout<<"  "<<current->left->token<<endl;
                        }
                    }

                    else { // Go to next S-exp
                        PrintSpace(level+1);
                        PrintSExp(current->left, level+1);
                    }
                }

                else if ( current -> type != LEFT_PAREN && current->type != Space ) { // Print dotted pair
                    PrintSpace(level);
                    cout<<"  ."<<endl;
                    PrintSpace(level);
                    if ( current->st == CONSTANT ) { // Print primptive
                        cout<<"  #<procedure "<<current->token<<">"<<endl;
                    }
                    else { // Print ATOM ( or ) QUOTE
                        if (current->type == FLOAT) {
                            string temp = current->token;
                            cout<<"  " << fixed << setprecision(3)<<stod(temp)<<endl;
                        }

                        else
                            cout<<"  "<<current->token<<endl;
                    }
                }
            }
        }

        void PrintSpace(int level) { // Print the space
            for (int i = 0; i < level; i++)
                cout<<"  ";
        }
        
        void Delete( shared_ptr<ASTNode> current ) { // Delete the AST tree
            current = nullptr;
        }

        ~Scanner() { // The destructor of the Parser class
            Delete(root);
        }
};
class Evaluation {
    private:
        unordered_set<string> primptive = {"atom?", "pair?", "list?", "null?", "integer?", "real?","number?", "string?", "boolean?", "symbol?", // type functions
                    "eqv?", "equal?", "+", "-", "*", "/", "<", ">", "<=", ">=", "=", "not", "and", "or",
                    "string>?", "string<?", "string=?", "string-append","set!", // string functions
                    "car", "cdr", "cons", "list", "quote", "define", "exit",  // list functions
                    "begin", "if", "cond", "clean-environment", "lambda", "let", "verbose", "verbose?",
                    "create-error-object", "error-object?", "read", "write", "eval", "set!", "display-string", "newline", // environment functions
                    "symbol->string", "number->string" // type conversion functions
                }; 
        unordered_map <string, shared_ptr<ASTNode>> defined_symbol;
        FormatError error;
        string errorSymbol; // The error symbol if got the worng format of the Symbol
        string errortoken;  // The error token if got the wrong format of the token
        struct Evaluate { // The structure of the Evaluate class
            int legal_args = 0; // The number of the legal arguments
            vector<string> variables; // The list of the parameters
            vector<shared_ptr<ASTNode>> Functions; // The function to be evaluated
        };
        unordered_map <string, Evaluate*> defined_function; // The list of the self_define functions
        unordered_map <string, Evaluate*> lambda_list; // The list of the lambda functions
        int lambda_level = -1; // The level of the lambda function
        bool verboseMode = true; // The verbose mode
        vector<string> lambda_args; // The list of the lambda arguments
        bool errorLock = false; // The error lock
        map<int, unordered_map<string, shared_ptr<ASTNode>>> local_variable_map; // The local variable map  
        int local_variable_level = -1; // The level of the local variable  
    public:
        shared_ptr<ASTNode> resultSExp = nullptr;
        shared_ptr<ASTNode> errorSEXP = nullptr;
        Evaluation() {error = FORMAT_CORRECT;} // The constructor of the Evaluation class}
    
    bool Error() { return error != FORMAT_CORRECT; }
    
    void SetError( FormatError e, string errorsym, string errorstr, shared_ptr<ASTNode> err ) { // Set the error
        error = e;             
        errorSymbol = errorsym;
        errortoken = errorstr;
        if (!errorLock ) { // 如果沒有錯誤的樹，才會進行錯誤的樹建構
            Delete(errorSEXP); // Delete the error tree
            errorSEXP = nullptr; // Reset the error tree

            if ( err == nullptr ) { 
                return; 
            }
            if ( error == UNEXPECTED_FORMAT || error == NON_LIST || error == NON_RETURN_VALUE || error == COND_ERROR ) {
                errorSEXP = ResetError(err); // Reset the error
            }
            else 
                errorSEXP = err;
        }   
    }
    
    FormatError getError() { return error; }

    shared_ptr<ASTNode> ResetError(shared_ptr<ASTNode> err ) { // Reset the error
        if ( err == nullptr ) { return nullptr; }
        shared_ptr<ASTNode> temp = nullptr;
        if ( err->st == CONSTANT ) {
            temp = make_shared<ASTNode>( err->type, ATOM, err->token, 0 );
        }

        else {
          temp = make_shared<ASTNode>( err->type, err->st, err->token, 0 );
        }
        
        temp -> left = ResetError(err->left);
        temp -> right = ResetError(err->right);
        return temp;
    }
    
    bool NOTList( shared_ptr<ASTNode>current ) {
        while ( current -> right ) { current = current -> right;} // 走到最後一個node
        return current -> left == nullptr; // 如果沒有left，代表不是list
    }

    void ResetLambdaFunc() { // Reset the lambda level
        lambda_level = -1;
        lambda_list.clear();
        local_variable_map.clear();
        local_variable_level = -1;
    }

    void EvalSExp( shared_ptr<ASTNode> root ) { // 接到建好的樹，進行執行
        errorLock = false; // Reset the error lock
        // <S-exp> ::= <ATOM>
        if ( !root -> left ) { // <ATOM>
            if ( root -> type == SYMBOL ) { // Symbol
                resultSExp = CopyTree(FindSymbol(root->token, root->st)); // Copy the tree                   
            }

            else { resultSExp = CopyTree(root); } // Nothing need to do( INT, FLOAT, STRING, NIL, T ) ATOM
        }

        // <S-exp> ::= ( <S-exp> )
        else { resultSExp = Eval( root ); } // Get the operation result
        ResetLambdaFunc(); // Reset the lambda level
        return;    
    }

    shared_ptr<ASTNode> Eval( shared_ptr<ASTNode> ASTRoot ) { // 遇到Symbol呼叫，進行運算值判斷，傳入參數為指令樹，回傳為結果樹
        shared_ptr<ASTNode> AnswerNode = nullptr; // 這個是來接每次sumbol的結果，如果有錯的話就一直傳一樣的。
        if ( Error() ) {return nullptr;} // 錯誤的Symbol Expression就會接到errorSExp的樹，之後的果一律變成nullptr
        // Search the corresponding function
        // 將計算的結果，整理整理存成一個新的node
        // 之後接到遞迴回傳的node後
        // 接在目前current的左邊( cons 接右邊 )
        // 之後再將current指向下一個節點
        if ( ASTRoot -> st == QUOTE ) { 
            return ASTRoot; 
        } // Quote
        if ( NOTList( ASTRoot ) ) { SetError( NON_LIST, "", "" , CopyTree(ASTRoot));return nullptr; }
        TokenType type = ASTRoot->left->type;
        string symbol = ASTRoot->left->token;
        shared_ptr<ASTNode> temp = nullptr;
        int level = ASTRoot->left->level;
        bool lambda_run = false; // Lambda function run
        if ( type == LEFT_PAREN ) { // S-exp
            temp = Eval( ASTRoot->left  );
            if ( Error() ) { 
                if ( error == NON_RETURN_VALUE ) {
                    SetError( NON_COND, "", "", CopyTree(ASTRoot->left) );
                    return nullptr; 
                }
                return nullptr; 
            }
            if ( temp->st == QUOTE ) { // If the function symbol is a quote, return error about ATTEMPT_NON_FUNCTION
                SetError( ATTEMPT_NON_FUNCTION, symbol, "", CopyTree(temp) );
                return nullptr;
            }
            if ( temp -> token != "lambda" )
                ASTRoot->left = temp; // 這邊的temp是指令的結果
            else {
                lambda_run = true;
            }
            type = temp->type;
            symbol = temp->token;
        }

        if ( type == SYMBOL ) { // Symbol
            string lambda_key = symbol; // For searching Lambda_list
            // Check if the symbol is a primitive function, if not is an error.
            temp = CopyTree(FindSymbol(symbol, ASTRoot->left->st )); // Find the symbol in the environment
            if ( temp == nullptr ) { return nullptr; } // No symbol found
            if ( temp->st == QUOTE ) { // If the function symbol is a quote, return error about ATTEMPT_NON_FUNCTION
                SetError( ATTEMPT_NON_FUNCTION, symbol, "", CopyTree(temp) );
                return nullptr;
            }

            type = temp->type; // Get the type of the symbol
            symbol = temp->token; // Get the symbol name

            vector<shared_ptr<ASTNode>> args = GetArgs( ASTRoot->right ); // Get the arguments
            if ( symbol == "define" ) { // Define 2
                if ( level >= 1 ) { 
                    SetError(LEVEL_DEFINE,"","",nullptr); return nullptr;}
                Define( args );
                if ( error == UNEXPECTED_FORMAT ) { // Define Error
                    SetError( UNEXPECTED_FORMAT, errorSymbol, errortoken, CopyTree(ASTRoot) );
                    return nullptr;
                }
                return nullptr;
            }
            else if ( symbol == "car" ) { AnswerNode = Car ( args ); } // Car 1
            else if ( symbol == "cdr" ) { AnswerNode = Cdr ( args ); } // Cdr 1
            else if ( symbol == "cons" ) { AnswerNode = Cons ( args ); } // Cons 2
            else if ( symbol == "list" ) { AnswerNode = List ( args ); } // List >=0
            else if ( symbol == "atom?" || symbol == "null?" || symbol == "integer?" || symbol == "real?" || symbol == "number?" || symbol == "string?" || symbol == "boolean?" || symbol =="symbol?" ) { // (Type)? 1
                if ( isWhat( args, symbol) ) { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( T, ATOM, "#t", 0 ); }
                else { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( NIL, ATOM, "nil", 0 );}
            }

            else if ( symbol == "pair?" || symbol == "list?" ) {  // Pair? List? 1
                if ( isPairOrList( args, symbol ) ) { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( T, ATOM, "#t", 0 ); }
                else { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); }
            }

            else if ( symbol == "eqv?" ) { // Eqv? 2
                if ( isEqv ( args ) ) { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( T, ATOM, "#t", 0 ); }
                else { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); }
            }

            else if ( symbol == "equal?" ) { // Equal? 2
                if ( isEqual ( args ) ) { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( T, ATOM, "#t", 0 ); }
                else { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); }
            }

            else if ( symbol == "+" || symbol == "-" || symbol == "*" || symbol == "/") { AnswerNode = Calculator( args ,symbol ); } // + - * / >=2
            else if ( symbol == "quote" ) { AnswerNode = Quote( args ); } // Quote 1
            else if ( symbol == "<" || symbol == ">" || symbol == "<=" || symbol == ">=" || symbol == "=" ) { // < >=2
                if ( Compare( args, symbol  ) ) { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( T, ATOM, "#t", 0 ); }
                else { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); }
            }
            else if ( symbol == "not" ) { AnswerNode = Not ( args ); } // Not 1
            else if ( symbol == "and" ) { AnswerNode = And ( args ); } // And >=2
            else if ( symbol == "or" ) { AnswerNode = Or ( args ); } // Or >=2
            else if ( symbol == "string>?" || symbol == "string<?" || symbol == "string=?" ) { // String>? >=2
                if ( CompareString( args, symbol  ) ) { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( T, ATOM, "#t", 0 ); }
                else { if ( Error() ) {return nullptr;} AnswerNode = make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); }
            }

            else if ( symbol == "string-append" ) { AnswerNode = StringAppend ( args );} // String-append >=2
            else if ( symbol == "if" ) { // If 2 or 3
                AnswerNode = If ( args );
                if ( Error() ) { // NO Error then return the result
                    if ( error == NON_RETURN_VALUE ) {
                        SetError( NON_RETURN_VALUE, "", "", CopyTree(ASTRoot) );
                    }
                    return nullptr;
                }
            }

            else if ( symbol == "cond" ) { // Cond >=1
                AnswerNode = Cond ( args );
                if ( Error() ) { // NO Error then return the result
                    if (  error == COND_ERROR ) {
                        SetError( error, "", "", CopyTree(ASTRoot) );
                        errorLock = true; // Lock the error tree
                    }
                    else if (error == NON_RETURN_VALUE )  // 將errorSexp變回原本呼叫的樣子
                        SetError( NON_RETURN_VALUE, "", "", CopyTree(ASTRoot) );
                    return nullptr;
                }
            }

            else if ( symbol == "begin" ) { 
                AnswerNode = Begin ( args );
                if ( Error() ) { // NO Error then return the result
                    if ( error == NON_RETURN_VALUE ) // 將errorSexp變回原本呼叫的樣子
                        SetError( NON_RETURN_VALUE, "", "", CopyTree(ASTRoot) );
                    return nullptr;
                }
            } // Begin >=1
            else if ( symbol == "exit" ) { // Exit 0
                if ( level >= 1 ) { SetError(LEVEL_EXIT,"","", nullptr); return nullptr; }
                Exit( args );
            }
            
            else if ( symbol == "clean-environment" ) { // Clean-Environment
                if ( level >= 1 ) { SetError(LEVEL_CLEAN,"","", nullptr); return nullptr; }

                defined_symbol.clear();
                defined_function.clear();
                ResetLambdaFunc();

                if (verboseMode) cout<<"environment cleaned"<<endl;
            }

            else if ( symbol == "lambda" ) { //
                // if ( local_function.find(lambda_key) != local_function.end() ) { // Defined Lambda by SYMBOL
                //     AnswerNode = SelfDefined( args, lambda_key  );
                //     if ( Error() ) { 
                //         if ( error == LAMBDA_ERROR ) {
                //             SetError( error, "lambda", "", CopyTree(ASTRoot) );
                //             errorLock = true; // Lock the error tree                  
                //         }
                //         else if (error == NON_RETURN_VALUE) // 將errorSexp變回原本呼叫的樣子
                //             SetError( error, "lambda", "", CopyTree(ASTRoot) );
                //         return nullptr;
                //     }
                // }
                if ( defined_function.find(lambda_key) != defined_function.end() ) { // Defined Lambda by SYMBOL
                    AnswerNode = SelfDefined( args, lambda_key  );
                    if ( Error() ) { 
                        if ( error == LAMBDA_ERROR ) {
                            SetError( error, "lambda", "", CopyTree(ASTRoot) );
                            errorLock = true; // Lock the error tree                  
                        }
                        else if (error == NON_RETURN_VALUE) { // 將errorSexp變回原本呼叫的樣子
                            SetError( error, "lambda", "", CopyTree(ASTRoot) );
                            local_variable_map.erase(local_variable_level); // Erase the local variable map
                            local_variable_level--;
                        }
                        return nullptr;
                    }
                } 
                else if ( lambda_run ) { // Evalute the Defined lambda function in lambda_list
                    AnswerNode = Lambda( args, "lambda"+to_string(lambda_level)  );
                    // lambda_list.erase("lambda"+to_string(lambda_level)); // Erase the lambda function after using it
                    if ( Error() ) { 
                        if ( error == LAMBDA_ERROR ) {
                            SetError( error, "lambda", "", CopyTree(ASTRoot) );
                            errorLock = true; // Lock the error tree                  
                        }
                        else if (error == NON_RETURN_VALUE) { // 將errorSexp變回原本呼叫的樣子
                            SetError( error, "lambda", "", CopyTree(ASTRoot) );
                            local_variable_map.erase(local_variable_level); // Erase the local variable map
                            local_variable_level--;
                        }
                        return nullptr;
                    }
                }
                
                else { // Go to define the lambda function
                    // ( lambda (x) ( + x x ) 1 ) -> print <#procedure lambda> 
                    // ( ( lambda (x) ( + x x ) ) 2 ) -> print the operated value ( This section -> Up section )
                    
                    AnswerNode = Define_Lambda( args ); // Basically print <#procedure lambda>
                    if ( Error() ) { SetError( LAMBDA_ERROR, "lambda", "", CopyTree(ASTRoot) ); errorLock = true; return nullptr;}
                }
            }

            else if ( symbol == "let" ) { // let >=2
                // ( let ( (x 1) (y 2) ) ( + x y ) ) -> print the operated value
                AnswerNode = Let ( args );
                if ( Error() ) { 
                    if ( error == LET_FORMAT ) {
                        SetError( error, "let", "", CopyTree(ASTRoot) );
                        errorLock = true; // Lock the error tree
                    }
                    else if ( error == NON_RETURN_VALUE ) { // 將errorSexp變回原本呼叫的樣子
                        SetError( error, "let", "", CopyTree(ASTRoot) );
                        local_variable_map.erase(local_variable_level); // Erase the local variable map
                        local_variable_level--;
                    }
                    return nullptr;
                }
            }  

            else if ( symbol == "verbose") {
                // False 代表關閉verbose模式
                // True 代表開啟verbose模式
                // 這邊要注意，verboseMode是全域變數，會影響到整個程式的運行
                AnswerNode = SetVerbose ( args );
                if ( Error()) {return nullptr;}
                verboseMode = AnswerNode->token == "#t" ? true : false;   
            }

            else if ( symbol == "verbose?") { // 目前的verboseMode狀態
                if ( args.size() != 0) {SetError( UNEXPECTED_ARGS_NUMBER, symbol, "", nullptr); return nullptr;}
                AnswerNode = make_shared<ASTNode>( verboseMode ? T : NIL, ATOM, verboseMode ? "#t" : "nil", 0 );
            }
            
            // 如果是自訂義變數
            else if ( defined_function.find( symbol ) != defined_function.end() ) { // 自訂義變數
                // 執行self_defined function
                AnswerNode = SelfDefined( args, symbol );
                if ( Error() ) {
                    if ( error == NON_RETURN_VALUE ) { // 將errorSexp變回原本呼叫的樣子
                        SetError( NON_RETURN_VALUE, "", "", CopyTree(ASTRoot) );
                        local_variable_map.erase(local_variable_level); // Erase the local variable map
                        local_variable_level--;
                    }
                    else if ( error == NON_COND ) {// 這個錯誤要抓原式
                        Delete(errorSEXP); // Delete the error tree
                        errorSEXP = nullptr;
                        errorLock = false;
                        SetError( error, "", "", CopyTree(ASTRoot) );
                        errorLock = true; // Lock the error tree
                    }
                    return nullptr;
                }
                // 有機會報什麼錯要寫
                // 1. 參數數量不正確
                // 2. 參數類型不正確
                // 3. 沒有return值
                //    3-1: 
            }
  
            else if ( symbol == "create-error-object" ) {
                AnswerNode = CreateErrorObject ( args );
            }

            else if ( symbol == "error-object?" ) {
                bool result = ErrorObject ( args );
                if ( Error() ) {return nullptr;}
                AnswerNode = make_shared<ASTNode>( result ? T : NIL, ATOM, result ? "#t" : "nil", 0 );
            }

            else if ( symbol == "read") {
                AnswerNode = Read ( args );
            }

            else if ( symbol == "write" ) {
                AnswerNode = Write ( args );
            }

            else if ( symbol == "display-string" ) {
                AnswerNode = DisplayString ( args );
            }

            else if ( symbol == "newline" ) {
                if ( args.size() != 0 ) { SetError( UNEXPECTED_ARGS_NUMBER, symbol, "", nullptr ); return nullptr; }
                cout<<endl;
                AnswerNode = make_shared<ASTNode>( NIL, ATOM, "nil", 0 );
            }

            else if ( symbol == "symbol->string" ) {
                AnswerNode = SymbolToString ( args );
            }

            else if ( symbol == "number->string") {
                AnswerNode = NumberToString ( args );
            }

            else if ( symbol == "set!" ) { // Set!
                AnswerNode = Set ( args );
            }

            else if ( symbol == "eval") {
                AnswerNode = eval ( args );
            }

            else {
                SetError( ATTEMPT_NON_FUNCTION, symbol, "", CopyTree( temp ) ); // If the function is not a symbol, return error about ATTEMPT_NON_FUNCTION
                return nullptr;
            }
        }
        
        else { SetError( ATTEMPT_NON_FUNCTION, symbol, "", CopyTree( ASTRoot->left ) ); return nullptr; } // If the function is not a symbol, return error about ATTEMPT_NON_FUNCTION

        if ( Error() ) return nullptr;

        return AnswerNode;
    }
    // Check the correct number of argument( All args, Account of args needed to check)
    vector<shared_ptr<ASTNode>> GetArgs( shared_ptr<ASTNode> current ) { 
        vector<shared_ptr<ASTNode>> args;
        if ( current == nullptr ) return args;
        for ( shared_ptr<ASTNode> temp = current; temp; temp = temp->right ) {
            if ( temp->left ) {
                args.push_back( temp->left );
            }
        }

        return args;
    }

    shared_ptr<ASTNode> FindSymbol( string symbol, StructureType st) { // Find the symbol in the defined_symbol or primptive
        if ( local_variable_level >= 0 && local_variable_map[local_variable_level].find(symbol) != local_variable_map[local_variable_level].end() ) { // Find the symbol in the variable_table
            return local_variable_map[local_variable_level][symbol];
        }

        else if ( defined_symbol.find( symbol ) != defined_symbol.end()) {
            shared_ptr<ASTNode> temp = defined_symbol[symbol];
            if ( primptive.find(temp-> token) != primptive.end() && temp-> st != QUOTE ) { // Find the symbol in the primptive
                return make_shared<ASTNode>( SYMBOL, CONSTANT, defined_symbol[symbol]->token ,0 );
            }

            else {
                return defined_symbol[symbol];
            }
        }

        else if ( defined_function.find( symbol ) != defined_function.end() ) { // Find the symbol in the defined_function
            return make_shared<ASTNode>( SYMBOL, CONSTANT, symbol, 0 );
        }

        else if ( primptive.find( symbol ) != primptive.end() && st != QUOTE ) { // Find the symbol in the primptive
            return make_shared<ASTNode>( SYMBOL, CONSTANT, symbol, 0 );
        }

        else if (  st == QUOTE ) {
            return make_shared<ASTNode>( SYMBOL, QUOTE, symbol, 0 );
        }

        else {
            SetError( UNBOUND_SYMBOL, symbol, "", nullptr);
            return nullptr;
        }
    }
 
    shared_ptr<ASTNode> CopyTree( shared_ptr<ASTNode> root ) { // Copy the tree
        if ( root == nullptr ) return nullptr;
        shared_ptr<ASTNode> temp = make_shared<ASTNode>( root->type, root->st, root->token, root->level );
        temp->left = CopyTree( root->left );
        temp->right = CopyTree( root->right );
        return temp;
    }

    shared_ptr<ASTNode> SymbolToString ( vector<shared_ptr<ASTNode>> args ) { // Convert the symbol to string
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "symbol->string", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) {
                    SetError(UNBOUND_PARAMETER, "", "", CopyTree(args[0]));
                }
            }
            return nullptr; 
        }
        if ( temp->type == SYMBOL ) {
            return make_shared<ASTNode>( STRING, ATOM,"\""+temp->token+"\"", 0 );
        }

        else {
            SetError(UNEXPECTED_TYPE, "symbol->string", "", CopyTree(temp));
            return nullptr;
        }

    }

    shared_ptr<ASTNode> NumberToString ( vector<shared_ptr<ASTNode>> args ) { // Convert the number to string
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "number->string", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) {
                    SetError(UNBOUND_PARAMETER, "", "", CopyTree(args[0]));
                }
            }
            return nullptr; 
        }
        if ( temp->type == INT  ) {
            return make_shared<ASTNode>( STRING, ATOM,"\""+temp->token+"\"", 0 );
        }
        else if ( temp->type == FLOAT ) {
            ostringstream ss;
            ss<<fixed<<setprecision(3)<<stod(temp->token);
            return make_shared<ASTNode>( STRING, ATOM,"\""+ss.str()+"\"" , 0 );
        }

        else {
            SetError(UNEXPECTED_TYPE, "number->string", "", CopyTree(temp));
            return nullptr;
        }
    }

    shared_ptr<ASTNode> Set( vector<shared_ptr<ASTNode>> args) { // Set the value of the variable
        if ( args.size() != 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "set!", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL ) {
            string symbol = args[0]->token;
            
            if ( args[1]->type == SYMBOL) {
                temp = FindSymbol( args[1]->token, args[1]->st  );
            }

            else if ( args[1]->type == LEFT_PAREN ) {
                temp = Eval( args[1]  );
            }

            else {
                temp = CopyTree( args[1] );
            }

            if ( temp == nullptr ) { 
                if ( Error() ) {
                    if ( error == NON_RETURN_VALUE) {
                        SetError(NON_RETURN_VALUE, "", "", CopyTree(args[1]));
                    }
                }
                return nullptr; 
            }

            int level = local_variable_level;
            while ( level >= 0 ) { // Check if the symbol is in the local variable map
                if ( local_variable_map[level].find(symbol) != local_variable_map[level].end() ) {
                    local_variable_map[level][symbol] = CopyTree(temp);
                    break;
                }
                level--;
            }

            if ( level < 0 ) {
                defined_symbol[symbol] = CopyTree(temp); // Set the value of the variable
            }
        }

        else {
            SetError( SET_FORMAT, "set!", "", CopyTree(args[0]) );
            return nullptr;
        }

        return CopyTree(temp);
    }

    shared_ptr<ASTNode> eval ( vector<shared_ptr<ASTNode>> args) { // Eval the expression
        // 還要修
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "eval", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        shared_ptr<ASTNode> exec = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) { SetError( UNBOUND_PARAMETER, "", "", CopyTree(args[0]));}
            }
            return nullptr; 
        }

        // 解封Quote且將level重設
        OpenQuote( temp, 0 );

        if ( temp->type == SYMBOL ) {
            exec = FindSymbol( temp->token, temp->st  );
        }

        else if ( temp->type == LEFT_PAREN ) {
            exec = Eval( temp );
        }

        else {
            exec = CopyTree( temp );
        }
        if ( exec == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) { SetError( UNBOUND_PARAMETER, "", "", CopyTree(temp));}
            }
            return nullptr; 
        }
        return CopyTree(exec);
    }

    void OpenQuote( shared_ptr<ASTNode> node, int level ) { // Open the quote
        if (node == nullptr) return;

        node -> level = level; // 更改level
        if (node->st == QUOTE) node->st = ATOM; // 如果這個節點是 QUOTE，就轉為 ATOM

        // 遞迴探索 left 和 right
        if (node->left) { 
            if ( node->left->type == LEFT_PAREN ) {
                OpenQuote(node->left, level + 1);
            }
            else {
                OpenQuote(node->left, level);
            }
        };
        if (node->right) OpenQuote(node->right, level);
    }

    shared_ptr<ASTNode> Read( vector<shared_ptr<ASTNode>> args) { // Read the input
        if ( args.size() != 0 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "read", "", nullptr );
            return nullptr;
        }

        Scanner s = Scanner();
        string expr = s.ReadSExp();
        if ( NextEOF ) {
            cout<<"Fuck EOF"<<endl;
        }
        shared_ptr<ASTNode> temp = s.root;
        if ( s.error != No_Error) {
            return GetErrorMessage( s.error, s.line , s.column, expr );
        }

        return Quote( vector<shared_ptr<ASTNode>>{temp} ); // Return the quoted expression
    }

    shared_ptr<ASTNode> Write( vector<shared_ptr<ASTNode>> args) { // Write the output
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "write", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) {
                    SetError(UNBOUND_PARAMETER, "", "", CopyTree(args[0]));
                }
            }
            return nullptr; 
        }
        WriteSEXP( temp, 0 );
        return temp;
    }

    void WriteSEXP( shared_ptr<ASTNode> current, int level ) { // Print the AST tree
        if ( current == nullptr ) { return; }
        if ( current->type != LEFT_PAREN && current->type != Space ) { // 只有一個node的情況
            cout<<PrintToken(current.get());
            return;
        }

        else {
            cout<<"( ";
            if (current->left-> type != LEFT_PAREN && current->left->type != Space   ) { // Print ATOM
                cout<<PrintToken(current->left.get())<<endl;
            }

            else { // Go to next S-exp
                WriteSEXP(current->left, level+1);
            }
            
            if ( current->right && (current -> right-> type != LEFT_PAREN && current->right->type != Space) ) { // Print ATOM ( dotted pair )
                PrintSpace(level);
                cout<<"  ."<<endl;
                PrintSpace(level);
                cout<<"  "<<PrintToken(current->right.get())<<endl;
            }

            else 
                PrintRight( current, level );

            PrintSpace(level);
            cout<<")";
            if ( level > 0 ) {
                cout<<endl;
            }
            return;
        }
    }
        
    void PrintRight( shared_ptr<ASTNode>current, int level ) {
        while (current->right) {
            current = current->right;
            if ( current -> type == Space ) { // 右邊節點print空白
                if ( current ->left-> type != LEFT_PAREN && current->left->type != Space ) { // Print ATOM or Quote
                    PrintSpace(level);
                    cout<<"  "<<PrintToken(current->left.get())<<endl;
                }

                else { // Go to next S-exp
                    PrintSpace(level+1);
                    WriteSEXP(current->left, level+1);
                }
            }

            else if ( current -> type != LEFT_PAREN && current->type != Space ) { // Print dotted pair
                PrintSpace(level);
                cout<<"  ."<<endl;
                PrintSpace(level);
                cout<<"  "<<PrintToken(current.get())<<endl;
            }
        }
    }

    string PrintToken( ASTNode* ans ) {
        if (  ans->type == FLOAT  ) {
            ostringstream ss;
            ss<<fixed<<setprecision(3)<<stod(ans->token);
            return ss.str();
        }

        if ( ans->st == CONSTANT ) {
            return "#<procedure "+ans->token+">";
        }

        return ans->token;
    }

    void PrintSpace ( int level ) {
        for ( int i = 0; i < level; i++ ) {
            cout<<"  ";
        }
    }
    
    shared_ptr<ASTNode> DisplayString( vector<shared_ptr<ASTNode>> args) { // Display the string
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "display-string", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) {
                    SetError(UNBOUND_PARAMETER, "", "", CopyTree(args[0]));
                }
            }
            return nullptr; 
        }
        if ( temp->type == STRING  || temp->type == ERROR ) {
            cout<<CutHair(temp->token);
        }

        else {
            SetError(UNEXPECTED_TYPE, "display-string", "", CopyTree(temp));
            return nullptr;
        }
        return temp;
    }

    shared_ptr<ASTNode> SetVerbose ( vector<shared_ptr<ASTNode>> args) { // Set the verbose mode
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "verbose", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) {
                    SetError(UNBOUND_PARAMETER, "", "", CopyTree(args[0]));
                }
            }
            return nullptr; 
        }
        if ( temp->type == NIL ) {
            return make_shared<ASTNode>( NIL, ATOM, "nil", 0 );
        }
        else {
            return make_shared<ASTNode>( T, ATOM, "#t", 0 );
        }
    }

    shared_ptr<ASTNode> GetErrorMessage( ErrorType error, int line, int column, string expr ) { // Get the error message
        if (error == UNEXPECTED_DOT) {
            return make_shared<ASTNode>(ERROR, ATOM ,"\"ERROR (unexpected token) : atom or '(' expected when token at Line "+to_string(line)+" Column "+to_string(column)+" is >>.<<\"", 0);
        }

        else if (error == UNEXPECTED_RIGHT_PAREN) {
            return make_shared<ASTNode>(ERROR, ATOM,"\"ERROR (unexpected token) : atom or '(' expected when token at Line "+to_string(line)+" Column "+to_string(column)+" is >>)<<\"", 0);
        }

        else if (error == UNEXPECTED_EOF){
            return make_shared<ASTNode>(ERROR, ATOM,"\"ERROR (no more input) : END-OF-FILE encountered\"", 0);
        }

        else if (error == UNEXPECTED_STRING) {
            return make_shared<ASTNode>( ERROR, ATOM, "\"ERROR (no closing quote) : END-OF-LINE encountered at Line "+to_string(line)+" Column "+to_string(column)+"\"", 0);
        }

        else if ( error == UNEXPECTED_TOKEN ){
            return make_shared<ASTNode>( ERROR, ATOM, "\"ERROR (unexpected token) : ')' expected when token at Line "+to_string(line)+" Column "+to_string(column)+" is >>"+expr+"<<\"", 0);
        } 

        return nullptr;
    }

    shared_ptr<ASTNode> CreateErrorObject( vector<shared_ptr<ASTNode>> args) { // Create the error object
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "create-error-object", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) {
                    SetError(UNBOUND_PARAMETER, "", "", CopyTree(args[0]));
                }
            }
            return nullptr; 
        }
        return make_shared<ASTNode>( ERROR, ATOM, temp->token, 0 );
    }

    bool ErrorObject( vector<shared_ptr<ASTNode>> args ) { // Check if the error object is valid
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "error-object?", "", nullptr );
            return false;
        }
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == SYMBOL) {
            temp = FindSymbol( args[0]->token, args[0]->st  );
        }

        else if ( args[0]->type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
        }

        else {
            temp = CopyTree( args[0] );
        }

        if ( temp == nullptr ) { 
            if ( Error() ) {
                if ( error == NON_RETURN_VALUE) {
                    SetError(UNBOUND_PARAMETER, "", "", CopyTree(args[0]));
                }
            }
            return false; 
        }
        return temp->type == ERROR ? true : false;
    }
    
    void Define( vector<shared_ptr<ASTNode>> args ) { // 2
        // 將第一個節點放進defined_Symbol
        // 第二個節點直接將left放進map[第一個節點]
        // cout<<第一個節點<<" defined<<endl;
        if ( args.size() < 2 ) {
            error = UNEXPECTED_FORMAT;
            return;
        }

        else {
            shared_ptr<ASTNode> args1 = args[0];
            shared_ptr<ASTNode> args2 = args[1];
            string func_name;
            if ( args1->type == LEFT_PAREN ) { // Define Sexp as a function ，這裡要改
                // 實作 ( define ( f x ) ( Sexp )) 
                // 將f的token改成定義的symbol, CONSTANT放進defined_symbol裡面
                // 將此自定義之參數數量，參數名稱，定義的Sexp存取進入defined_function裡面(一個new Struct)
                func_name = args1->left->token; // 愈被定義的function名稱
                if ( args1->left->type != SYMBOL || primptive.find( func_name ) != primptive.end() ) { // Define Non-Symbol as a function is error
                    SetError( UNEXPECTED_FORMAT, "define", "", nullptr );
                    return;
                }

                Evaluate *e = new Evaluate();
                for ( shared_ptr<ASTNode> temp = args[0] -> right ; temp; temp = temp->right ) { // 走到最後一個node
                    if ( temp -> left == nullptr) {
                        SetError( UNEXPECTED_FORMAT, "define", "", nullptr);
                        return;
                    }
                    if ( temp -> left -> type != SYMBOL ) { // 參數必須是symbol
                        SetError( UNEXPECTED_FORMAT, "define", "", nullptr);
                        return;
                    }
        
                    e->legal_args++;
                    e->variables.push_back( temp->left->token );
                }
        
                for ( int i = 1; i < args.size(); i++ ) { // 走到最後一個node
                    e->Functions.push_back( CopyTree( args[i] ) );
                }
        
                defined_function[func_name] = e; 
                e = nullptr;
                delete e;

                defined_symbol[func_name] = make_shared<ASTNode>( SYMBOL, CONSTANT, func_name, 0 ); // 將function名稱放進去
            }

            else if ( args1->type != SYMBOL ) { // Define Non-Symbol as a function is error
                error = UNEXPECTED_FORMAT;
                return;
            }

            else if ( primptive.find( args1->token ) != primptive.end() ) { // Define Symbol as\" a primitive function is error
                error = UNEXPECTED_FORMAT;
                return;
            }

            else { // Define SYMBOL as ATOM, Sexp, PRIMITIVE, defined SYMBOL
                if ( args.size() != 2 ) {
                    error = UNEXPECTED_FORMAT;
                    return;
                }

                func_name = args1->token; // 愈被定義的function名稱
                if ( args2->type == LEFT_PAREN ) { // Define Symbol as a Sexp
                    shared_ptr<ASTNode> temp = nullptr;
                    temp = Eval( args2 ); // Eval the Sexp
                    if ( temp == nullptr ) {
                        return;
                    }
                    if ( args2->left->token == "lambda" ) { // Define Symbol as a lambda function
                        Evaluate *e = lambda_list["lambda"+to_string(lambda_level)]; // 使用Lambda_level來判斷目前使用哪個lambda函數 
                        defined_function[func_name] = e; // 將lambda_list["lambda"]的東西存進去
                        lambda_list.erase("lambda"+to_string(lambda_level));
                        lambda_level--; // Decrease the lambda level
                    }
                    
                    defined_symbol[func_name] = temp;
                }

                else if ( args2->type == SYMBOL && args2->st != QUOTE ) { // Define Symbol as a Symbol
                    if ( primptive.find( args2->token ) != primptive.end() ) { // Define Symbol as a primitive function
                        defined_symbol[func_name] = make_shared<ASTNode>( SYMBOL, CONSTANT, args2->token, 0 );
                    } 

                    else if ( defined_function.find( args2->token ) != defined_function.end() ) { // It's a defined function
                        defined_symbol[func_name] = defined_symbol[args2->token]; // Define Symbol as a defined function
                        defined_function[func_name] = defined_function[args2->token]; // Define Symbol as a defined function
                    }

                    else if ( defined_symbol.find( args2->token ) == defined_symbol.end() ) { // Define Symbol as an unbound Symbol is an error
                        SetError( UNBOUND_SYMBOL, args2->token, "", nullptr );
                        return;
                    }

                    else {
                        defined_symbol[func_name] = defined_symbol[args2->token];
                    } // Define Symbol as a defined Symbol
                }

                else { 
                    defined_symbol[func_name] = CopyTree( args2 );
                 } // Define Symbol as a ATOM
            } 

            if ( verboseMode ) cout<<func_name<<" defined"<<endl;
        }
        
        return;
    }

    shared_ptr<ASTNode> Define_Lambda( vector<shared_ptr<ASTNode>> args) { // >= 2 ( lambda (x) ( + x x ) 1 ) -> lambda
        // 記錄lambda裡的東西存在lambda_list的哪裡
        if ( args.size() < 2) {
            SetError( LAMBDA_ERROR, "lambda", "", nullptr );
            return nullptr;
        }

        if ( args[0]->type != LEFT_PAREN && args[0]->type != NIL) { // 如果第一個參數不是list，則報錯
            SetError( LAMBDA_ERROR, "lambda", "", nullptr );
            return nullptr;
        }

        Evaluate *e = new Evaluate(); // 這邊的lambda_list["lambda"]是指令的結果
        for ( shared_ptr<ASTNode> temp = args[0]; temp; temp = temp->right ) { // 走到最後一個node
            if ( temp -> type == NIL) {
                e->legal_args = 0; // 如果沒有參數，則不需要參數
                break;
            }

            if ( temp -> left -> type != SYMBOL ) { // 參數必須是symbol
                SetError( LAMBDA_ERROR, "lambda", "", nullptr);
                return nullptr;
            }

            e->legal_args++;
            e->variables.push_back( temp->left->token );
        }

        for ( int i = 1; i < args.size(); i++ ) { // 走到最後一個node to get all the args which are functions
            e->Functions.push_back( CopyTree( args[i] ) );
        }

        lambda_level++; // Increase the lambda level
        lambda_list["lambda"+to_string(lambda_level)] = e; // 將lambda_list["lambda"]的東西存進去
        // defined_function["lambda"] = e; // 將lambda_list["lambda"]的東西存進去
        e = nullptr; // 這邊的lambda_list["lambda"]是指令的結果
        delete e;
        
        return make_shared<ASTNode>( SYMBOL, CONSTANT, "lambda", 0 ); // 這邊要改成lambda_list裡面的東西   
    }

    shared_ptr<ASTNode> Lambda( vector<shared_ptr<ASTNode>> args, string lambda_name) { // >= 2 ( (lambda (x) ( + x x )) 1 ) -> ( lambda 1 )
        Evaluate *e = lambda_list[lambda_name]; 
        lambda_list.erase(lambda_name); // Erase the lambda function after using it
        lambda_level--; // Decrease the lambda level
        if ( e == nullptr ) { SetError( LAMBDA_ERROR, "lambda", "", nullptr ); return nullptr; } // 如果沒有lambda_list，則回傳nullptr
        // 記錄lambda裡的東西存在lambda_list的哪裡
        if ( args.size() != e->legal_args ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "lambda", "", nullptr );
            return nullptr;
        }   

        shared_ptr<ASTNode> result = nullptr;
        unordered_map<string, shared_ptr<ASTNode>> variables_table; // lambda的變數表
        for (  int i = 0; i < e->legal_args; i++ ) {
            if ( args[i]->type == LEFT_PAREN ) {
                shared_ptr<ASTNode> temp = Eval( args[i]  ); // Eval the Sexp
                if ( temp == nullptr ) { 
                    if (error == NON_RETURN_VALUE) { // variable assigned to a non-variable
                        errorLock = true; // Set the error lock to true
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[i]) );
                    }
                    return nullptr; 
                } 
                // if ( temp->token == "lambda" ) {
                //     local_function[e->variables[i]] = lambda_list["lambda"+to_string(lambda_level)]; // 將lambda_list["lambda"]的東西存進去
                //     lambda_list.erase("lambda"+to_string(lambda_level)); // Erase the lambda function after using it
                //     lambda_level--; // Decrease the lambda level
                // }
                
                variables_table[e->variables[i]] = CopyTree( temp ); // 將lambda的參數換成變數值
            }

            else if ( args[i]->type == SYMBOL ) { // 如果是symbol，則直接放進去
                shared_ptr<ASTNode> temp = FindSymbol( args[i]->token, args[i]->st  ); // Eval the Sexp
                if ( temp == nullptr ) { return nullptr; } 
                variables_table[e->variables[i]] = temp; // 將lambda的參數換成變數值
            }

            else
                variables_table[e->variables[i]] = CopyTree( args[i] ); // 將lambda的參數換成變數值
        }

        local_variable_level++; // Increase the local variable level
        local_variable_map[local_variable_level] = variables_table; // 將lambda的變數表存進去
 
        // 計算
        for ( shared_ptr<ASTNode> func : e->Functions ) { 
            Delete( result ); // 先刪除之前的結果
            result = nullptr;
            // shared_ptr<ASTNode> temp = ChangedFunction( func, variables_table ); // 將function的參數換成變數值
            //if ( temp == nullptr ) { return nullptr; } // 如果有錯誤，則回傳nullptr
            if ( func -> type == LEFT_PAREN ) { // 執行Sexp in lambda
                // cout << "Lambda:
                result = Eval( func  ); // 執行Lambda
                if ( Error() ) { 
                    if (error == NON_RETURN_VALUE) {
                        // Delete(errorSEXP); // Delete the error tree
                        // errorSEXP = nullptr;
                        errorLock = false; // Reset the error lock
                        SetError( FORMAT_CORRECT, "", "", nullptr ); // Reset the error
                        // cout << "Skip the Non-return value error in Lambda" << endl;
                        continue; // 這邊要繼續執行下一個function
                    }
                    return nullptr; 
                } // No Error then return the result
            }

            else if ( func -> type == SYMBOL ) { // 如果是Symbol，則要FindSymbol
                result = CopyTree(FindSymbol( func->token, func->st));
                if ( Error() ) { return nullptr; } // No Error then return the result
            }

            else { // 如果是ATOM，則直接回傳
                result = CopyTree( func );
            }
        }

        if ( result == nullptr) { // 回到Eval，回傳(<lambda> () ())的tree
            SetError( NON_RETURN_VALUE,"", "", nullptr );
            return nullptr; 
        } // 如果有錯誤，則回傳nullptr
        local_variable_map.erase(local_variable_level); // 清空變數表
        local_variable_level--; // Decrease the local variable level
        return result; // 回傳結果
    }

    shared_ptr<ASTNode> SelfDefined( vector<shared_ptr<ASTNode>> args, string function_name) { // >= 2 自訂義function, ( define ( f x ) ( Sexp ) ) ( f 2 )
        // 自訂義的function，應存入defined_function裡面
        // 而defined_symbol裡要存入對應的function_key, ( SYMBOL, CONSTANT, )
        // 之後在Eval的時候，會去defined_function裡面找對應的function_key
        Evaluate *e ;
        bool local = false; // 判斷是否是local function
        // if ( local_function.find( function_name ) != local_function.end() ) { // 如果是local function
        //     e = local_function[function_name];
        //     local = true; // 設定local function
        // }
        // else 
        if ( defined_function.find( function_name ) != defined_function.end() ) { // 如果是自訂義function
            e = defined_function[function_name];
        }
        else {
            SetError( UNBOUND_SYMBOL, function_name, "", nullptr );
            return nullptr;
        }
        int legal_args = e->legal_args;
        vector<string> variables = e->variables;
        vector<shared_ptr<ASTNode>> functions = e->Functions;

        if ( args.size() != legal_args ) {
            if ( local )
                SetError( UNEXPECTED_ARGS_NUMBER, "lambda", "", nullptr );
            else
                SetError( UNEXPECTED_ARGS_NUMBER, defined_symbol[function_name]->token, "", nullptr );
            return nullptr;
        }

        // 取變數名稱及其值

        unordered_map<string, shared_ptr<ASTNode>> temp_values;
        vector<string> lambda_vars; // 被定義為lambda的變數
        // 第一階段：獲得所有args（不能修改 variables_table）
        for ( int i = 0; i < legal_args; i++ ) {
            temp_values[variables[i]] = CopyTree( args[i] ); // 將lambda的參數換成變數值
        }

        // 計算arg的值，根據var的先後順序
        for ( auto &var : variables ) {
            shared_ptr<ASTNode> value = nullptr;
            if ( temp_values[var]->type == SYMBOL ) {
                string token = temp_values[var]->token;
                shared_ptr<ASTNode> temp = FindSymbol( token, temp_values[var]->st ); // Eval the Sexp
                if ( temp == nullptr ) { return nullptr; } // No Error then return the result
                // if ( temp->token == "lambda" ) { // 使用同一個Function
                //     Evaluate *e = defined_function[token];
                //     local_function[var] = e;
                // }
                value = temp; // 將lambda的參數換成變數值
            }

            else if ( temp_values[var]->type == LEFT_PAREN ) { // 如果是Sexp，則要Eval
                value = Eval( temp_values[var]  );
                if ( Error() ) { 
                    if (error == NON_RETURN_VALUE) { // variable assigned to a non-variable
                        errorLock = true; // Set the error lock to true
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(temp_values[var]) );
                    }
                    return nullptr; 
                } // No Error then return the result
                // if ( value -> token == "lambda" ) { // 如果是lambda，則要Eval
                //     Evaluate *e = lambda_list["lambda"+to_string(lambda_level)]; // 使用Lambda_level來判斷目前使用哪個lambda函數 
                //     local_function[var] = e; // 將lambda_list["lambda"]的東西存進去
                //     lambda_list.erase("lambda"+to_string(lambda_level));
                //     lambda_level--; // Decrease the lambda level
                //     lambda_vars.push_back(var); // 將lambda的參數換成變數值
                // }
            }
            else {
                value = CopyTree( temp_values[var] );
            }

            temp_values[var] = value;
        }

        local_variable_level++; // Increase the local variable level
        local_variable_map[local_variable_level] = temp_values; // 將lambda的變數表存進去
        // for ( string arg : lambda_args ) {
        //     if ( local_variable_map[local_variable_level].find(arg) != local_variable_map[local_variable_level].end() ) { // 如果變數表裡有這個變數，則不需要再計算
        //         .erase(arg);
        //     }
        // }

        // if ( local_variable_map[local_variable_level].find(function_name) != local_variable_map[local_variable_level].end() ) { // 如果變數表裡有這個變數，則不需要再計算
        //     local_variable_map[local_variable_level].clear(); // 清空變數表
        //     local_variable_map[local_variable_level].clear(); // 清空local function
        // }

        // // 第二階段：真正更新變數表
        // // 如果此函數是lambda結構(在variable_table裡存取lambda)裡面，則清空變數表
        // for ( int i = 0; i < legal_args; i++ ) {
        //     local_variable_map[local_variable_level][variables[i]] = temp_values[variables[i]];
        // }
        
        // 計算
        shared_ptr<ASTNode> result = nullptr;

        // 可能不只一組function，所以要用for迴圈
        for ( shared_ptr<ASTNode> func : functions ) { 
            
            Delete(result); // Delete the result tree
            result = nullptr; // Reset the result tree
            if ( func -> type == LEFT_PAREN ) { // 執行Sexp in user function
                result = Eval( func  );
                if ( Error() ) { 
                    if (error == NON_RETURN_VALUE) { // 跳過Non-return value的錯誤
                        // Delete(errorSEXP); // Delete the error tree
                        // errorSEXP = nullptr;
                        errorLock = false; // Reset the error lock
                        SetError( FORMAT_CORRECT, "", "", nullptr ); // Reset the error
                        // cout << "Skip the Non-return value error in Self defined" << endl;
                        continue;
                    }
                    return nullptr; 
                } // No Error then return the result
            }

            else if ( func -> type == SYMBOL ) { // 如果是Symbol，則要FindSymbol
                result = FindSymbol( func->token, func->st );
                if ( Error() ) { return nullptr; } // No Error then return the result
            }

            else { // 如果是ATOM，則直接回傳
                result = CopyTree( func );
            }
        }
        if (result == nullptr) { // 回傳Eval，回傳(<user Funcion> ())的tree
            SetError( NON_RETURN_VALUE,"", "", nullptr );
            return nullptr; 
        } // No Error then return the result
        // for ( string arg : lambda_vars ) {
        //     if ( defined_function.find(arg) != defined_function.end() ) { // 如果變數表裡有這個變數，則不需要再計算
        //         defined_function.erase(arg);
        //     }
        // }
        local_variable_map.erase(local_variable_level); // 清空變數表
        local_variable_level--; // Decrease the local variable level
        return result; 
    }

    shared_ptr<ASTNode> Let( vector<shared_ptr<ASTNode>> args ) { // >=2
        if ( args.size() < 2 ) {
            SetError( LET_FORMAT, "let", "", nullptr );
            return nullptr;
        }
        // 直接執行宣告變數，代換運算
        // arg[0] 為所有變數及其值的pair
        // arg[1] 為運算式
        // ( let ( ( x 1 ) ( y 2 ) ) ( + x y )) -> print 3

        // 取變數名稱及其值
        unordered_map<string, shared_ptr<ASTNode>> temp_values;
        vector<string> lambda_vars; // 被定義為lambda的變數
        vector<string> variables; // 變數順序
        for ( shared_ptr<ASTNode> para = args[0]; para; para = para -> right ) { // 走到最後一個node將變數名稱及其值存入變數表
            if ( para -> type == NIL) { break;} // 沒有變數名稱及其值，則跳出迴圈
            shared_ptr<ASTNode> parameter = para->left; // 取得變數名稱及其值
            if ( parameter == nullptr ) { // 參數不能是空的
                SetError( LET_FORMAT, "let", "", nullptr);
                return nullptr;
            }

            else if ( parameter -> type != LEFT_PAREN && parameter->type != Space ) { // Error: (let ( x 2 ) ...)
                SetError( LET_FORMAT, "let", "", nullptr);
                return nullptr;
            }
            
            string var_name = parameter->left->token; // 變數名稱
            if ( parameter->left-> type != SYMBOL ) { // 參數必須是symbol
                SetError( LET_FORMAT, "let", "", nullptr);
                return nullptr;
            }

            if (  primptive.find( var_name ) != primptive.end() ) { // 參數不能是primitive function
                SetError( LET_FORMAT, "let", "", nullptr);
                return nullptr;
            }
            
            vector<shared_ptr<ASTNode>> para_arg = GetArgs( parameter->right ); // 取得變數值
            if ( para_arg.size() != 1 ) { // 變數值必須只有一個
                SetError( LET_FORMAT, "let", "", nullptr);
                return nullptr;
            }
            temp_values[var_name] = CopyTree( para_arg[0] ); // 將變數名稱及其值存入變數表
            variables.push_back(var_name); // 將變數名稱存入變數表
        }

        // 計算參數值，根據var的先後順序
        for ( auto& var : variables ) { // 將變數名稱及其值存入變數表
            if ( temp_values[var] == nullptr ) { // 參數不能是空的
                SetError( LET_FORMAT, "let", "", nullptr);
                return nullptr;
            }
            if ( temp_values[var]->type == LEFT_PAREN ) { // 如果是Sexp，則要Eval，再塞入參數表
                shared_ptr<ASTNode> temp = Eval( temp_values[var]  ); // Eval the Sexp then become the value of variable
                if ( temp == nullptr ) { 
                    if (error == NON_RETURN_VALUE) { // variable who assigned by Non-return value
                            // Delete(errorSEXP); // Delete the error tree
                            // errorSEXP = nullptr; // Reset the error tree
                        SetError( NON_Value_Variable,"", "", CopyTree(temp_values[var]) ); // 如果參數是空的，則報錯
                        errorLock = true; // Lock the error tree
                    }
                    return nullptr; 
                } // No Error then return the result
                // if ( temp -> token == "lambda" ) { // 如果是lambda，則要Eval
                //     Evaluate *e = lambda_list["lambda"+to_string(lambda_level)]; // 使用Lambda_level來判斷目前使用哪個lambda函數 
                //     local_function[var] = e; // 將lambda_list["lambda"]的東西存進去
                //     lambda_list.erase("lambda"+to_string(lambda_level));
                //     lambda_level--; // Decrease the lambda level
                //     lambda_vars.push_back(var); // 將lambda的參數換成變數值
                // }

                temp_values[var] = CopyTree( temp ); // 將變數名稱及其值存入變數表
            }

            else if ( temp_values[var]->type == SYMBOL ) { // 如果是Symbol，則要FindSymbol
                shared_ptr<ASTNode> temp = FindSymbol( temp_values[var]->token, temp_values[var]->st  ); // Eval the Sexp
                if ( temp == nullptr ) { return nullptr; } // No Error then return the result
                temp_values[var] =  temp ; // 將變數名稱及其值存入變數表
            }
            else { // 如果是ATOM，則直接回傳
                temp_values[var] = CopyTree( temp_values[var] );
            }
        }
        unordered_map<string, shared_ptr<ASTNode>> local_variable_map_temp;
        if ( local_variable_level < 0 ) {
            local_variable_map_temp = temp_values; // 取得local_variable_map
        }
        else {
            for ( auto& var : local_variable_map[local_variable_level] ) { // 取得local_variable_map
                local_variable_map_temp[var.first] = var.second; // 將變數名稱及其值存入變數表
            }

            for ( auto& var : temp_values ) {
                if ( local_variable_map_temp.find(var.first) != local_variable_map_temp.end() ) { // 如果在變數表中找到重複的變數名稱，則將其刪除
                    local_variable_map_temp.erase(var.first); // 刪除變數表中的let的參數
                }

                local_variable_map_temp[var.first] = var.second; // 將變數名稱及其值存入變數表
            }            
        }

        local_variable_level++; // Increase the local variable level
        local_variable_map[local_variable_level] = local_variable_map_temp; // 將變數名稱及其值存入新的變數表
        // // variable_table有的變數可能是let的參數，因此要將其刪除
        // if ( local_variable_map[local_variable_level].empty() ) { // 如果沒有變數表，則不需要刪除
        //     local_variable_map[local_variable_level] = temp_values; // 將變數名稱及其值存入變數表
        // }

        // else { // 如果變數表有，則要將其刪除

        // }

        // 將function的參數換成變數值
        shared_ptr<ASTNode> result = nullptr;
        for ( int i = 1; i < args.size(); i++ ) { // 執行到最後一個node
            Delete(result); // Delete the result tree
            result = nullptr; // Reset the result tree
            // shared_ptr<ASTNode> evalFunction = ChangedFunction( args[i], variables_table); // 將function的參數換成變數值;
            // if ( evalFunction == nullptr ) { return nullptr; } // No Error then return the result
            if ( args[i] -> type == LEFT_PAREN ) { // 執行Sexp in Let
                result = Eval( args[i]  ); // 執行Let
                if ( Error() ) { 
                    if (error == NON_RETURN_VALUE) { // 跳過non_return_value的錯誤
                        // Delete(errorSEXP); // Delete the error tree
                        // errorSEXP = nullptr;
                        errorLock = false; // Reset the error lock
                        SetError( FORMAT_CORRECT, "", "", nullptr ); // Reset the error
                        // cout << "Skip the Non-return value error in Let" << endl;
                        continue;
                    }
                    return nullptr; 
                } // No Error then return the result
            }
            else if ( args[i] -> type == SYMBOL ) { // 如果是Symbol，則要FindSymbol
                result = CopyTree( FindSymbol( args[i]->token, args[i]->st  ) ); // Eval the Sexp
                if ( Error() ) { return nullptr; } // No Error then return the result
            }

            else { // 如果是ATOM，則直接回傳
                result = CopyTree( args[i] );
            }
            // Delete(evalFunction); // Delete the evalFunction tree
            // evalFunction = nullptr; // Reset the evalFunction tree
        }
        if ( result == nullptr ) { // 回到Eval，回傳(Let (()) () ()) 的tree
            SetError( NON_RETURN_VALUE,"", "", nullptr);
            return nullptr; 
        } // No Error then return the result
        
        local_variable_map.erase(local_variable_level); // 清空變數表
        local_variable_level--; // Decrease the local variable level
        return result; 
    }

    shared_ptr<ASTNode> Car( vector<shared_ptr<ASTNode>> args  ) { // 1 Dotted Pair Left
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "car", "", nullptr );
            return nullptr;
        }

        // args[0] -> left,  Only accept List, Dotted Pair
        shared_ptr<ASTNode> result = nullptr;
        
        if ( args[0] -> type == LEFT_PAREN ) { 
            result = Eval( args[0] ); } // List or Dotted Pair

        else if ( args[0] -> type == SYMBOL ) { 
            result = CopyTree(FindSymbol( args[0]->token, args[0]->st  )); } // Symbol defined as List or Dotted Pair

        else {
            SetError( UNEXPECTED_TYPE, "car","",CopyTree(args[0]) );
            return nullptr;
        }

        if ( result == nullptr ) {
            if (error == NON_RETURN_VALUE) {
                SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[0]) );
            }
            return nullptr;
        } // Get the Error of operate Sexp

        else if ( result -> left == nullptr ) { // 只有一個node，視為error。
            SetError( UNEXPECTED_TYPE, "car","", CopyTree(result) );
            return nullptr;
        }

        else { return CopyTree( result -> left ); } // 回傳左子樹
    }

    shared_ptr<ASTNode> Cdr( vector<shared_ptr<ASTNode>> args  ) { // 1 
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "cdr", "",nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> result = nullptr;
        shared_ptr<ASTNode> temp = nullptr;
        // args[0] -> right,  Only accept List, Dotted Pair
        // First get a temp to know what will the args be
        if ( args[0] -> type == LEFT_PAREN ) { // List or Dotted Pair
            temp = Eval( args[0] );

        }

        else if ( args[0] -> type == SYMBOL ) { // Symbol defined as List or Dotted Pair
            temp = CopyTree(FindSymbol( args[0]->token, args[0]->st  ));  
        }

        else {
            SetError( UNEXPECTED_TYPE, "cdr", "" ,CopyTree(args[0]));
            return nullptr;
        }

        if ( temp == nullptr ) {
            if (error == NON_RETURN_VALUE) {
                SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[0]) );
            }
            return nullptr;
        } // Get the Error of operate Sexp

        else if ( !temp -> left && !temp -> right ) { // 只有一個node，視為error。
            SetError( UNEXPECTED_TYPE, "cdr","", CopyTree(temp) );
            return nullptr;
        }
        
        else if ( temp -> right == nullptr ) { result = make_shared<ASTNode>( NIL, ATOM, "nil", 0 );} // 右邊是nil

        else if ( temp -> right -> type == Space  ) { // 右邊是一個S-exp ， 需轉換樹的print結構
            result = CopyTree( temp -> right );
            result -> type = LEFT_PAREN;
            result -> st = S_EXP;
        }

        else { result = CopyTree( temp -> right ); } // 右邊單純是一個ATOM

        return result;
    }

    shared_ptr<ASTNode> Cons( vector<shared_ptr<ASTNode>> args) { // 2 Put two args into dotted pair
        if ( args.size() != 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "cons", "", nullptr );
            return nullptr;
        }

        shared_ptr<ASTNode> result = make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", 0 );
        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0]->type == LEFT_PAREN ) { 
            temp = Eval( args[0] ); // Cons Left operates a function);
            if ( temp == nullptr ) {
                if (error == NON_RETURN_VALUE) {
                    SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[0]) );
                }
                return nullptr;
            } // Get the Error of operate Sexp
        } // Cons Left operates a function

        else if ( args[0] -> type == SYMBOL ) { 
            temp = CopyTree(FindSymbol( args[0]->token, args[0]->st  )); // Find and Insert the symbol in the defined_symbol or primptive
            if ( temp == nullptr ) return nullptr; // Find the defined symbol
        } // Cons Left gets defined symbol

        else temp = CopyTree( args[0] ); // Cons Left gets other types of ATOM

        if ( temp  == nullptr) { return nullptr; }

        result -> left = temp; // Insert the left side of the dotted pair

        temp = nullptr;
        // Right side of the dotted pair ( No null)
        if ( args[1]->type == LEFT_PAREN ) { 
            temp = Eval( args[1] ); // Cons Right operates a function
            if ( temp == nullptr ) {
                if (error == NON_RETURN_VALUE) {
                    SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[1]) );
                }
                return nullptr;
            }
        } // Cons Right gets operated function

        else if ( args[1] -> type == SYMBOL ) { // Cons Right gets defined symbol
            temp = CopyTree(FindSymbol( args[1]->token, args[1]->st  ));
            if ( temp == nullptr ) return nullptr;
        }

        else temp = CopyTree( args[1] );

        if ( temp -> type != NIL) { // Right side of the dotted pair ( No null )
            if ( temp -> type == LEFT_PAREN) {
                temp -> type = Space;
                temp -> st = NONE;
            }   
        }
        else { temp = nullptr; } // Right side of the dotted pair ( null )

        result -> right = temp;
        return result;
    }

    shared_ptr<ASTNode> List( vector<shared_ptr<ASTNode>> args  ) { // >=0
        if ( args.size() == 0 ) { return make_shared<ASTNode>( NIL, ATOM, "nil", 0 );}

        shared_ptr<ASTNode> result = make_shared<ASTNode>( LEFT_PAREN, S_EXP, "(", 0 );
        shared_ptr<ASTNode> temp = nullptr;
        for (  shared_ptr<ASTNode> arg : args ) {
            if ( arg->type == LEFT_PAREN ) { 
                temp = Eval( arg ); // List operates a function); 
                if ( temp == nullptr ) {
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(arg) );
                    }
                    return nullptr;
                } // Get the Error of operate Sexp
            } // Insert the operated Sexp function

            else if ( arg -> type == SYMBOL ) { 
                temp = CopyTree(FindSymbol( arg->token , arg->st  )); } // Find and Insert the symbol in the defined_symbol or primptive

            else { temp = CopyTree( arg );} // Another types just copy the tree and insert it.

            if ( temp == nullptr ) {return nullptr;}

            result->Insert( temp, temp->st, 0 );
        }

        return result;
    }

    bool isPairOrList( vector<shared_ptr<ASTNode>> args, string cal  ) { // 1
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, cal, "", nullptr );
            return false;
        }

        shared_ptr<ASTNode> temp = nullptr;
        if ( args[0] -> type == LEFT_PAREN ) { 
            temp = Eval( args[0]  );
            if ( temp == nullptr ) {
                if (error == NON_RETURN_VALUE) {
                    SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[0]) );
                }
                return false;
            } // Get the Error of operate Sexp
        } // Get the operate Sexp

        else if ( args[0] -> type == SYMBOL ) {
            temp = CopyTree(FindSymbol( args[0]->token, args[0]->st  )); // Find and Insert the symbol in the defined_symbol or primptive
            if ( temp == nullptr ) return false; // Find the defined symbol
        } // Get the defined symbol

        else { 
            if ( cal == "list?" && args[0]->type == NIL ) { return true; } // If the args is nil, then is a list.
            return false; 
        } // Not a List or Dotted Pair

        // Check if the result is a List or Dotted Pair
        if ( temp == nullptr ) return false; // 這邊的temp是指令的錯誤

        if ( cal == "pair?" ) { return temp -> left != nullptr; } // Only if left is not null, then is pair.

        else if ( cal == "list?" ) { 
            if ( !temp -> left && !temp->right && temp->type == NIL ) { return true; } // If the args is nil, then is a list.
            if ( temp -> left == nullptr ) 
                return false; 
            return !NOTList( temp );
        } // If every nodes have left node, then is a list. 

        return false;
    }
    // Determine the type of the argument
    bool isWhat( vector<shared_ptr<ASTNode>> args, string cal  ) { // 1
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, cal, "" , nullptr );
            return false;
        }

        else {
            TokenType type = args[0]->type;
            shared_ptr<ASTNode> temp = nullptr;
            if ( args[0]->type == LEFT_PAREN ) { // 先把SEXP算出來，在得到Type
                temp = Eval( args[0]  );
                if ( temp == nullptr ) {
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[0]) );
                    }
                    return false;
                }
                type = temp->type;
            }

            else if ( args[0]->type == SYMBOL ) { // 先把SEXP算出來，在得到Type
                temp = CopyTree(FindSymbol( args[0]->token, args[0]->st  )); // Find and Insert the symbol in the defined_symbol or primptive
                if ( temp == nullptr ) return false; // Find the defined symbol
                if ( temp -> st == CONSTANT ) 
                    type = Procedure;
                else
                    type = temp->type;
            }

            if ( cal == "null?" ) {
                if ( type == NIL ) return true;
                else return false;
            }
            else if ( cal == "atom?" )  {
                if ( type == LEFT_PAREN ) return false;
                else return true;
            }
            else if ( cal == "integer?" ) {
                if ( type == INT ) return true;
                else return false;
            }            

            else if ( cal == "number?" || cal == "real?" ) {
                if ( type == INT || type == FLOAT ) return true;
                else return false;
            }

            else if ( cal == "string?" ) {
                if ( type == STRING || type == ERROR ) return true;
                else return false;
            }

            else if ( cal == "boolean?" ) {
                if ( type == T || type == NIL ) return true;
                else return false;
            }

            else if ( cal == "symbol?" ) {
                if ( type == SYMBOL ) return true;
                else return false;
            }

            else {
                SetError( UNEXPECTED_TYPE, cal,"", CopyTree(args[0]) );
                return false;
            }
        }
    }
    // Compare the value of the argument
    bool isEqv( vector<shared_ptr<ASTNode>> args  ) { // 2
        if ( args.size() != 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "eqv?", "", nullptr );
            return false;
        }
        // String 不可用eqv
        if( args[0] -> type == STRING || args[1] -> type == STRING ) {
            return false;
        }

        // 如果是單純數字就直接比較
        shared_ptr<ASTNode> temp1 = nullptr;
        shared_ptr<ASTNode> temp2 = nullptr;
        bool GOTFirst = false;
        double num1 = 0.0;
        double num2 = 0.0;
        for ( shared_ptr<ASTNode> arg : args ) { // Get two argus
            if ( arg -> type == LEFT_PAREN ) {
                if ( arg->left -> token == "lambda") {
                    return false; // Lambda cannot be compared
                }

                temp2 = Eval( arg  ); // Get the operated Sexp
                if ( temp2 == nullptr ) {
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(arg) );
                    }
                    return false;
                }
            }

            else if ( arg -> type == SYMBOL ) {
                temp2 = FindSymbol( arg->token, arg->st );
                if ( temp2 == nullptr ) return false; // 找不到symbol的話，報錯
            }

            else {
                temp2 = CopyTree( arg );
            }

            if ( temp2 -> type == INT || temp2 -> type == FLOAT ) {
                num2 = stod(temp2->token); // Convert to string
                if ( !GOTFirst ) {
                    num1 = num2;
                    temp1 = temp2;
                    temp2 = nullptr;
                    GOTFirst = true;
                    continue;
                }
                else if ( num1 == num2 ) { return true; } // Compare the value of the atom
                else { return false; } // Compare the value of the atom
            }

            if ( !GOTFirst ) {
                temp1 = temp2;
                temp2 = nullptr;
                GOTFirst = true;
                continue;
            }
        }

        if ( temp1->type != temp2->type ) { return false; }
        if ( temp1->type == LEFT_PAREN ) { return temp1 == temp2 && CompareList(temp1, temp2); }
        else if (temp1 -> type == STRING ) {return temp1 == temp2;} // Compare ther memory address
        else { return temp1->token == temp2->token; } // Directly compare the value

        return false;
    }

    bool isEqual( vector<shared_ptr<ASTNode>> args  ) { // 2
        if ( args.size() != 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "equal?", "", nullptr );
            return false;
        }

        shared_ptr<ASTNode>temp1 = nullptr;
        shared_ptr<ASTNode>temp2 = nullptr;
        bool GOTFirst = false;
        double num1 = 0.0;
        double num2 = 0.0;
        for ( shared_ptr<ASTNode> arg : args ) {
            if ( arg -> type == LEFT_PAREN ) { // Get the operated Sexp
                if ( arg->left -> token == "lambda") {
                    return false; // Lambda cannot be compared
                }
                temp2 = Eval(arg  );
                if ( temp2 == nullptr ) { 
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(arg) );
                    }
                    return false;
                } // 這邊的temp是指令的錯誤
            }

            else if ( arg -> type == SYMBOL ) { // Get the defined symbol
                temp2 = CopyTree(FindSymbol( arg->token, arg->st  )); 
                if ( temp2 == nullptr ) return false; // 找不到symbol的話，報錯
            }

            else { temp2 = CopyTree(arg); }

            if ( temp2 -> type == INT || temp2 -> type == FLOAT ) {
                num2 = stod(temp2->token); // Convert to string
                if ( !GOTFirst ) {
                    num1 = num2;
                    temp1 = temp2;
                    temp2 = nullptr;
                    GOTFirst = true;
                    continue;
                }
                else if ( num1 == num2 ) { return true; } // Compare the value of the atom
                else { return false; } // Compare the value of the atom
            }

            if ( !GOTFirst ) {
                temp1 = temp2;
                temp2 = nullptr;
                GOTFirst = true;
                continue;
            }
        }

        if ( temp1->type != temp2->type ) {return false;} // Compare the type of the atom

        else if ( temp1 -> type == LEFT_PAREN ) { return CompareList( temp1, temp2 ); } // Compre the value in ASTtree

        else {return temp1->token == temp2->token; } // Compare the value of the atom

        return false;
    }

    bool CompareList( shared_ptr<ASTNode> arg1, shared_ptr<ASTNode>arg2 ) {
        if ( arg1 == nullptr && arg2 == nullptr ) {return true;}

        if ( arg1 == nullptr || arg2 == nullptr ) {return false;}

        if ( arg1->type != arg2->type ) { return false;}

        if ( arg1->type == LEFT_PAREN || arg1 -> type == Space) { return CompareList( arg1->left, arg2->left ) && CompareList( arg1->right, arg2->right );}

        else {return arg1->token == arg2->token;}

        return false;
    }

    shared_ptr<ASTNode> Calculator( vector<shared_ptr<ASTNode>> args , string cal  ) { // 2 or more, Insert the ALU( +, -, *, / )
        shared_ptr<ASTNode> result = nullptr;
        if ( args.size() < 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, cal, "", nullptr );
            return nullptr;
        }

        double num = 0.0;
        bool isFloat = false;
        bool GetNum = false;
        for ( shared_ptr<ASTNode> arg : args ) { 
            double num2 = 0.0;
            if ( arg -> type == INT ) { num2 = stoi(arg->token); } // int

            else if ( arg-> type == FLOAT ) { num2 = stof(arg->token); isFloat = true; } // Float

            else if ( arg -> type == SYMBOL ) { // Get the defined symbol
                shared_ptr<ASTNode> temp = CopyTree(FindSymbol( arg->token, arg->st  ));
                if ( temp == nullptr ) return nullptr; // 找不到symbol的話，報錯
                if ( temp -> type == INT ) { num2 = stoi(temp->token); }
                else if ( temp -> type == FLOAT ) { num2 = stof(temp->token); isFloat = true;}
                else { SetError( UNEXPECTED_TYPE, cal, "", CopyTree( temp ) ); return nullptr;}
            }

            else if ( arg ->type == LEFT_PAREN ) { // Get the operated Sexp
                shared_ptr<ASTNode> temp = Eval( arg  );
                if ( temp == nullptr ) {
                    if (error == NON_RETURN_VALUE ) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(arg) );
                    }
                    return nullptr;
                } // 這邊的temp是指令的錯誤
                if ( temp -> type == INT ) { num2 = stoi(temp->token);} // int
                else if ( temp -> type == FLOAT ) {num2 = stof(temp->token); isFloat = true;} // float
                else { SetError( UNEXPECTED_TYPE, cal, "", CopyTree(temp) ); return nullptr; }
                Delete(temp);
            }

            else { SetError( UNEXPECTED_TYPE, cal,"",CopyTree( arg ) ); return nullptr;}

            if ( !GetNum ) {
                num = num2;
                GetNum = true;
                continue;
            }

            if ( cal == "+" ) { num += num2; }

            else if ( cal == "-" ) { num -= num2; }

            else if ( cal == "*" ) { num *= num2; }

            else if ( cal == "/" ) { 
                if ( num2 == 0 ) { SetError( DIVISION_ZERO, cal, "/", nullptr ); return nullptr; }
                num /= num2;
            }
        }
    
        if ( Error() ) return nullptr;

        if ( isFloat ) { return make_shared<ASTNode>( FLOAT, ATOM, to_string(num), 0 ); } // 回傳float

        else { return make_shared<ASTNode>( INT, ATOM, to_string(static_cast<int>(num)), 0 );} // 回傳int
    }

    shared_ptr<ASTNode> Not( vector<shared_ptr<ASTNode>> args  ) { // 1
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "not", "", nullptr );
            return nullptr;
        }

        if ( args[0]->type == NIL ) { return make_shared<ASTNode>( T, ATOM, "#t", 0 ); } // nil的話，回傳#t

        else if ( args[0]->type == SYMBOL ) { // Get the defined symbol
            shared_ptr<ASTNode> temp = CopyTree(FindSymbol( args[0]->token, args[0]->st  )); 
            if ( temp == nullptr ) return nullptr; // 找不到symbol的話，報錯
            if ( temp->type == NIL ) { Delete(temp); return make_shared<ASTNode>( T, ATOM, "#t", 0 ); } // 是nil的話，回傳#t
            else { Delete(temp); return make_shared<ASTNode>( NIL, ATOM, "nil", 0 );} // 不是nil的話，回傳nil
        }

        else if ( args[0]->type == LEFT_PAREN ) { // Get the operated Sexp
            shared_ptr<ASTNode> temp = Eval( args[0]  );
            if ( temp == nullptr ) {
                if (error == NON_RETURN_VALUE && errorSEXP != nullptr) {
                    SetError( UNBOUND_PARAMETER,"", "", CopyTree(args[0]) );
                }
                return nullptr; 
            }// 這邊的temp是指令的錯誤
            if ( temp->type == NIL ) { Delete(temp); return make_shared<ASTNode>( T, ATOM, "#t", 0 ); } // 是nil的話，回傳#t
            else { Delete(temp); return make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); } // 不是nil的話，回傳nil
        }

        else { return make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); } // 其他的話，視為nil
    }

    shared_ptr<ASTNode> And( vector<shared_ptr<ASTNode>> args  ) { // 2 or more
        if ( args.size() < 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "and", "", nullptr );
            return nullptr;
        }

        shared_ptr<ASTNode> temp = nullptr;
        for ( shared_ptr<ASTNode> arg : args ) {
            Delete (temp); // Delete the previous temp
            if ( arg -> type == NIL ) { return make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); }

            else if ( arg -> type == SYMBOL ) {
                temp = CopyTree(FindSymbol( arg->token, arg->st  ));
                if ( temp == nullptr ) return nullptr; // 找不到symbol的話，報錯
                if ( temp -> type == NIL ) {return temp;} // 是nil的話，直接回傳nil
            }

            else if ( arg -> type == LEFT_PAREN ) { // Got the operated Sexp
                temp = Eval( arg  );
                if ( temp == nullptr ) {
                    if (error == NON_RETURN_VALUE && errorSEXP != nullptr) {
                        SetError( UNBOUND_COND,"", "", CopyTree(arg) );
                    }
                    return nullptr;
                } // 這邊的temp是指令的錯誤
                if ( temp -> type == NIL ) { return temp; } // 是nil的話，直接回傳nil
            }

            else { temp = CopyTree( arg ); } // 不是nil的話，保存此點
        }

        return temp; 
    }

    shared_ptr<ASTNode> Or( vector<shared_ptr<ASTNode>> args  ) { // 2 or more
        if ( args.size() < 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "or", "", nullptr );
            return nullptr;
        }

        for ( shared_ptr<ASTNode> arg : args ) {
            if ( arg -> type == NIL ) { continue; }

            else if ( arg -> type == SYMBOL ) {
                shared_ptr<ASTNode> temp = CopyTree(FindSymbol( arg->token, arg->st  ));
                if ( temp == nullptr ) return nullptr; // 找不到symbol的話，報錯
                if ( temp -> type != NIL ) { return CopyTree( temp );} // 不是nill，就會回傳此點
            }

            else if ( arg -> type == LEFT_PAREN ) {
                shared_ptr<ASTNode> temp = Eval( arg  );
                if ( temp == nullptr ) {
                    if (error == NON_RETURN_VALUE && errorSEXP != nullptr) {
                        SetError( UNBOUND_COND,"", "", CopyTree(arg) );
                    }
                    return nullptr;
                }
                if ( temp -> type != NIL ) { return CopyTree( temp ); }
                Delete(temp);
            }

            else { return CopyTree( arg ); } // 不是nil的話，直接回傳此點
        }

        return make_shared<ASTNode>( NIL, ATOM, "nil", 0 ); // 都沒有nil的話，回傳nil
    }
    // Comparing the ATOMs
    bool Compare( vector<shared_ptr<ASTNode>> args, string cmp  ) { // >=2
        if ( args.size() < 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, cmp, "", nullptr );
            return false;
        }
        vector<double> numList;
        for ( shared_ptr<ASTNode> arg : args ) {
            double num2 = 0.0;
            if ( arg -> type == SYMBOL ) { // Get the defined symbol
                shared_ptr<ASTNode> temp = CopyTree( FindSymbol( arg->token, arg->st  ));
                if ( temp == nullptr ) return false; // 找不到symbol的話，報錯
                if ( temp -> type == INT || temp->type == FLOAT ) { num2 = stod(temp->token); } // Int or float
                else { SetError( UNEXPECTED_TYPE, cmp, "", CopyTree(temp) ); return false; } // 不是int或float的話，報錯
            }

            else if ( arg -> type == INT || arg -> type == FLOAT  ) { num2 = stod(arg->token); } // Int or float

            else if ( arg -> type == LEFT_PAREN ) { // Get the operated Sexp
                shared_ptr<ASTNode> temp = Eval( arg  );
                if ( temp == nullptr ) {
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(arg) );
                    }

                    return false;
                }
                if ( temp -> type == INT || temp -> type == FLOAT ) { num2 = stod(temp->token); } // Int or float
                else { SetError( UNEXPECTED_TYPE, cmp, "", CopyTree(temp) ); return false; } // 不是int或float的話，報錯
            }

            else { SetError( UNEXPECTED_TYPE, cmp, "", CopyTree(arg) ); return false; } // 不是int或float的話，報錯

            numList.push_back(num2);
        }

        double num1 = 0.0;
        bool GotFirst = false;
        for ( double num2 : numList ) {
            if ( !GotFirst ) {
                num1 = num2;
                GotFirst = true;
                continue;
            }

            if ( cmp == "<" ) {
                if ( num1 >= num2 ) {
                    return false;
                }
            }

            else if ( cmp == ">" ) {
                if ( num1 <= num2 ) {
                    return false;
                }
            }

            else if ( cmp == "<=" ) {
                if ( num1 > num2 ) {
                    return false;
                }
            }

            else if ( cmp == ">=" ) {
                if ( num1 < num2 ) {
                    return false;
                }
            }

            else if ( cmp == "=" ) {
                if ( num1 != num2 ) {
                    return false;
                }
            }

            num1 = num2;
        }

        return true;
    }

    bool CompareString( vector<shared_ptr<ASTNode>> args, string stringcmp  ) {
        if ( args.size() < 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, stringcmp, "", nullptr );
            return false;
        }

        vector<string> strList;
        for ( shared_ptr<ASTNode> arg : args ) {
            string str2;
            if ( arg -> type == SYMBOL ) {
                shared_ptr<ASTNode> temp = CopyTree(FindSymbol( arg->token, arg->st  ));
                if ( temp == nullptr ) return false; // 找不到symbol的話，報錯
                if ( temp->type == STRING ) {str2 = CutHair(temp->token);}
                else {SetError( UNEXPECTED_TYPE, stringcmp, "", CopyTree(temp) ); return false;} // 不是string的話，報錯
            }

            else if ( arg -> type == STRING || arg -> type == ERROR) { str2 = CutHair(arg->token); } // string

            else if ( arg -> type == LEFT_PAREN ) { // Sexp 
                shared_ptr<ASTNode> temp = Eval( arg );
                if ( temp == nullptr ) {
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(arg) );
                    }
                    return false;
                }
                if ( temp -> type == STRING || temp->type == ERROR ) { str2 = CutHair(temp->token);}

                else { SetError( UNEXPECTED_TYPE, stringcmp, "", CopyTree(temp) ); return false; }
            }
            
            else { SetError( UNEXPECTED_TYPE, stringcmp, "", CopyTree(arg) ); return false; } // 不是string的話，報錯

            strList.push_back(str2);
        }

        string str1;
        string str2;
        bool GotFirst = false;
        for ( string str2 : strList ) {

            if ( !GotFirst ) {
                str1 = str2;
                GotFirst = true;
                continue;
            }

            if ( stringcmp == "string>?" ) { 
                if ( str1 <= str2 ) { return false; }
            }

            else if ( stringcmp == "string<?" ) {
                if ( str1 >= str2 ) { return false;}
            }

            else if ( stringcmp == "string=?" ) {
                if ( str1 != str2 ) { return false;}
            }

            str1 = str2;
        }

        return true;
    }

    string CutHair( string str ) {
        str.erase(0, 1);
        str.erase(str.length()-1, 1);
        return str;
    }

    shared_ptr<ASTNode> StringAppend( vector<shared_ptr<ASTNode>> args  ) { // 2 or more 將所有的string合併
        string result;
        shared_ptr<ASTNode> resultNode = nullptr;
        if ( args.size() < 2 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "string-append", "", nullptr );
            return nullptr;
        }

        // 先檢查每個args的型態，然後將string合併
        for ( shared_ptr<ASTNode> arg : args ) {
            shared_ptr<ASTNode> temp = arg;
            if ( temp->type == STRING || temp->type == ERROR ) { result.append(CutHair(temp->token)); }// String的話，直接append

            else if ( temp->type == SYMBOL ) {
                temp = CopyTree(FindSymbol( temp->token, arg->st  )); // 找到symbol的話，報錯
                if ( temp == nullptr ) return nullptr; // 找不到symbol的話，報錯
                if ( temp->type == STRING ) { result.append(CutHair(temp->token)); } // String的話，直接append

                else { SetError( UNEXPECTED_TYPE, "string-append", "", CopyTree(temp) ); return nullptr;} // 不是string的話，報錯
            }

            else if ( temp->type == LEFT_PAREN ) {
                temp = Eval( arg  ); // 先算出來
                if ( Error ()) { 
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_PARAMETER,"", "", CopyTree(arg) );
                    }
                    return nullptr;
                } // 算不出來的話，報錯

                if ( temp->type == STRING || temp->type == ERROR ) { result.append(CutHair(temp->token));} // String的話，直接append

                else { SetError( UNEXPECTED_TYPE, "string-append", "", CopyTree(temp) ); return nullptr; } // 不是string的話，報錯
            }

            else { SetError( UNEXPECTED_TYPE, "string-append", "", CopyTree(arg) ); return nullptr; } // 不是string的話，報錯
        }

        result.insert(0, "\"");
        result.append("\"");
        resultNode = make_shared<ASTNode>( STRING, ATOM, result, 0 );
        return resultNode;
    }
    // Quote
    shared_ptr<ASTNode> Quote( vector<shared_ptr<ASTNode>> args ) { // 1 需要想一下怎麼處理
        if ( args.size() != 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "quote", "", nullptr );
            return nullptr;
        }

        for ( shared_ptr<ASTNode> temp = args[0]; temp; temp = temp->right ) {
            temp->st = QUOTE;
            if (temp->left) {
                temp->left->st = QUOTE;
            }
        }

        return CopyTree(args[0]);
    }
    // Conditions (if, cond, begin)
    shared_ptr<ASTNode> If( vector<shared_ptr<ASTNode>> args  ) { // 2 or 3
        if ( args.size() < 2 || args.size() > 3 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "if", "", nullptr );
            return nullptr;
        }

        // 當args[0]為NIL時，回傳args[2]，否則回傳args[1]
        // 若無args[2]則報錯No return value
        vector<shared_ptr<ASTNode>> condition;
        shared_ptr<ASTNode> temp = nullptr;
        TokenType type = args[0]->type;
        
        if ( type == LEFT_PAREN ) {
            temp = Eval( args[0]  );
            if ( temp == nullptr ) {
                if (error == NON_RETURN_VALUE) {
                    SetError( UNBOUND_TEST_COND,"", "", CopyTree(args[0]) );
                }
                return nullptr;
            }
            type = temp->type;
        }

        else if ( type == SYMBOL ) {
            temp = CopyTree(FindSymbol( args[0]->token, args[0]->st  ));
            if ( temp == nullptr ) return nullptr;
            type = temp->type;
        }
        

        int value = 0;
        if ( type == NIL ) {
            if ( args.size() == 3 ) {
                value = 2;
            }

            else { // 回到Eval函數，複製(if () ()) 的tree
                SetError( NON_RETURN_VALUE, "if", "", nullptr );
                return nullptr;
            }
        }

        else {
            value = 1;
        }

        if ( args[value]->type == LEFT_PAREN ) {
            temp = Eval( args[value]  );
            if ( temp == nullptr ) { return nullptr;}
        }
        else if ( args[value]->type == SYMBOL ) {
            temp = CopyTree(FindSymbol( args[value]->token, args[value]->st ) );
            if ( temp == nullptr ) return nullptr;
        }

        else {
            temp = CopyTree( args[value] );
        }

        return temp;
    }

    shared_ptr<ASTNode> Cond( vector<shared_ptr<ASTNode>> args  ) { // 1 or more
        if ( args.size() < 1 ) {
            SetError( COND_ERROR, "cond", "", nullptr );
            return nullptr;
        }

        shared_ptr<ASTNode> result = nullptr;
        // 確保每個條件都是( ... )組成

        for ( int i = 0; i < args.size(); i++ ) {
            if ( args[i] -> type != LEFT_PAREN || !args[i] -> right ) { // 右邊必須有東西
                SetError( COND_ERROR, "cond", "", nullptr );
                return nullptr;
            }
        }
        bool GotTrue = false;
        // 獲取判斷條件
        // 第一個是NIL，則繼續下一個
        for ( int i = 0; i < args.size(); i++ ) {    
            shared_ptr<ASTNode> temp1 = args[i] -> left; 
            if ( temp1 -> type == SYMBOL ) { // Symbol 就轉換
                if ( i == args.size()-1 ) {
                    if ( temp1->token == "else" ) {
                        temp1 = make_shared<ASTNode>( T, ATOM, "#t", 0 );
                    }

                    else  {
                        temp1 = CopyTree(FindSymbol( temp1->token, temp1->st  ));
                        if ( temp1 == nullptr ) return nullptr; // 找不到symbol的話，報錯
                    }
                }

                else  {
                    temp1 = CopyTree (FindSymbol( temp1->token, temp1->st  ) );
                    if ( temp1 == nullptr ) return nullptr; // 找不到symbol的話，報錯
                }
            }

            else if ( temp1 -> type == LEFT_PAREN ) { // 遞迴計算
                shared_ptr<ASTNode> temp2 = Eval( temp1  );
                if ( Error() ) {
                    if (error == NON_RETURN_VALUE) {
                        SetError( UNBOUND_TEST_COND,"", "", CopyTree(temp1) );
                    }
                    return nullptr;
                }

                temp1 = temp2;
            }
            
            if ( temp1 -> type == NIL ) { continue; }

            // 執行此判斷式後方的S-exp

            vector<shared_ptr<ASTNode>> subarg = GetArgs( args[i]->right );
            for ( shared_ptr<ASTNode> arg : subarg ) {
                Delete(result); // Delete the previous result
                if ( arg -> type == LEFT_PAREN ) {
                    result = Eval( arg  );
                    if ( result == nullptr ) {
                        if (error == NON_RETURN_VALUE) {
                            // Delete(errorSEXP);
                            // errorSEXP = nullptr;
                            errorLock = false;
                            SetError( FORMAT_CORRECT, "", "", nullptr );
                            // cout << "Skip the Non-return value error in COND" << endl;
                            continue;
                        }
                        return nullptr;
                    }
                }
                else if ( arg -> type == SYMBOL ) {
                    result = CopyTree(FindSymbol( arg->token, arg->st  ));
                    if ( result == nullptr ) return nullptr; // 找不到symbol的話，報錯
                }

                else {
                    result = CopyTree( arg );
                }
            }

            break;
        }

        if ( result == nullptr ) { // 回到Eval函數，複製(cond () () ()) 的tree
            SetError( NON_RETURN_VALUE, "cond", "", nullptr );
            return nullptr;
        }

        return result;
    }

    shared_ptr<ASTNode> Begin( vector<shared_ptr<ASTNode>> args  ) { // 1 or more
        if ( args.size() < 1 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "begin", "", nullptr );
            return nullptr;
        }
        shared_ptr<ASTNode> result = nullptr;
        for ( shared_ptr<ASTNode> arg : args ) {
            Delete(result);
            result = nullptr;
            if ( arg -> type == LEFT_PAREN ) {
                result = Eval( arg  );
                if ( result == nullptr ) {
                    if (error == NON_RETURN_VALUE) { // 跳過NON_RETURN_VALUE的錯誤
                        // Delete(errorSEXP);
                        // errorSEXP = nullptr;
                        errorLock = false;
                        SetError( FORMAT_CORRECT, "", "", nullptr );
                        // cout << "Skip the Non-return value error in BEGIN" << endl;
                        continue;
                    }
                    return nullptr;
                }
            }

            else if ( arg -> type == SYMBOL ) {
                result = CopyTree(FindSymbol( arg->token, arg->st  ));
                if ( result == nullptr ) return nullptr;
            }

            else {
                result = CopyTree( arg );
            }
        }

        if ( result == nullptr ) { // 回到Eval函數，複製(begin () () ()) 的tree
            SetError( NON_RETURN_VALUE, "begin", "", nullptr );
            return nullptr;
        }

        return result;
    }
    
    void Exit( vector<shared_ptr<ASTNode>> args ) { // 0
        if ( args.size() != 0 ) {
            SetError( UNEXPECTED_ARGS_NUMBER, "exit", "", nullptr );
            return;
        }

        else {
            cout<<"Thanks for using OurScheme!"<<endl;
            exit(0);
        }
    }
    
    void PrintEvaluationError( ) {
        if ( error == UNBOUND_SYMBOL ) {
            cout<<"ERROR (unbound symbol) : "<<errorSymbol<<endl;
        }

        else if ( error == UNEXPECTED_TYPE ) {
            cout<<"ERROR ("<<errorSymbol<<" with incorrect argument type) : ";
        }

        else if ( error == UNEXPECTED_ARGS_NUMBER ) {
            cout<<"ERROR (incorrect number of arguments) : "<<errorSymbol<<endl;
        }

        else if ( error == ATTEMPT_NON_FUNCTION ) {
            cout<<"ERROR (attempt to apply non-function) : ";
        }

        else if ( error == DIVISION_ZERO ) {
            cout<<"ERROR (division by zero) : "<<errorSymbol<<endl;
        }

        else if ( error == UNEXPECTED_FORMAT ) {
            cout<<"ERROR (DEFINE format) : "; // Print the tree with PrintSExp  in main Function
        }

        else if ( error == NON_RETURN_VALUE ) {
            cout<<"ERROR (no return value) : ";
        }

        else if ( error == NON_COND) {
            cout<<"ERROR (no return value) : ";
            error = NON_RETURN_VALUE;
        }

        else if ( error == NON_Value_Variable ) {
            cout<<"ERROR (no return value) : ";
            error = NON_RETURN_VALUE;
        }

        else if ( error == COND_ERROR ) {
            cout<<"ERROR (COND format) : ";
        }

        else if ( error == NON_LIST ) {
            cout<<"ERROR (non-list) : ";
        }

        else if ( error == LEVEL_DEFINE ) {
            cout<<"ERROR (level of DEFINE)"<<endl;
        }

        else if ( error == LEVEL_CLEAN ) {
            cout<<"ERROR (level of CLEAN-ENVIRONMENT)"<<endl;
        }

        else if ( error == LEVEL_EXIT ) {
            cout<<"ERROR (level of EXIT)"<<endl;
        }

        else if ( error == LAMBDA_ERROR ) {
            cout<< "ERROR (LAMBDA format) : ";
        }

        else if ( error == LET_FORMAT ) {
            cout<< "ERROR (LET format) : ";
        }
        
        else if ( error == SET_FORMAT) {
            cout<< "ERROR (SET! format) : ";
        }

        else if ( error == UNBOUND_PARAMETER) {
            cout<<"ERROR (unbound parameter) : ";
        }

        else if ( error == UNBOUND_COND) {
            cout<< "ERROR (unbound condition) : ";
        }

        else if ( error == UNBOUND_TEST_COND) {
            cout<< "ERROR (unbound test-condition) : ";
        }

        else {
            return;
        }
    }
    
    void Delete( shared_ptr<ASTNode> current ) {
        current = nullptr;
    }
    
    void Reset() {
        Delete( resultSExp );
        resultSExp = nullptr;
        errorSEXP = nullptr;
        error = FORMAT_CORRECT;
        errorSymbol.clear();
        errortoken.clear();
    }
};
int main() {
    char test_number;
    cin.get(test_number);
    char enter;
    cin.get(enter);
    Evaluation eval;
    cout<<"Welcome to OurScheme!"<<endl;
    while (true) {
        eval.SetError( FORMAT_CORRECT, "", "", nullptr );
        cout<<endl<<"> ";
        string expr;
        Scanner scanner;
        if ( NextEOF ) { // Check if the input is EOF
            cout<<"ERROR (no more input) : END-OF-FILE encountered"<<endl;
            break;
        }
        else
            expr = scanner.ReadSExp(); // Read the S-expression
        if (expr == "(exit)") { // Exit the program
            cout<<endl;
            break;
        }

        if ( scanner.error != No_Error ) { // IF NO Syntax Error
            scanner.PrintErrorMessage(expr); // Print the Error S-expression
        }

        else { 
            // Can go forward to Evaluation(Project 2)
            eval.EvalSExp(scanner.root);
            if ( eval.getError() != FORMAT_CORRECT ) { // Format Error
                eval.PrintEvaluationError();
                // if ( eval.getError() == NON_LIST || eval.getError() == UNEXPECTED_FORMAT || 
                //         eval.getError() == COND_ERROR || eval.getError() == NON_RETURN_VALUE ) {
                scanner.PrintSExp(eval.errorSEXP, 0); // Print the AST Tree of Error
                // }
            }

            else {
                scanner.PrintSExp(eval.resultSExp, 0); // Print the Result of S-expression
            }
            // scanner.PrintSExp(scanner.root, 0); // Print the S-expression
        }

        if ( scanner.error == UNEXPECTED_EOF ) {
            // scanner.PrintSExp(scanner.root, 0); // Print the S-expression
            break;
        }

        eval.Reset( ); // Delete the AST tree of result
    }

    cout<<"Thanks for using OurScheme!"<<endl;
    return 0;
}