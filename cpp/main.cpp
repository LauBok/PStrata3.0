#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <vector>
using namespace std;

vector<string> split (string s, string delimiter) {
    size_t pos_start = 0, pos_end, delim_len = delimiter.length();
    string token;
    vector<string> res;

    while ((pos_end = s.find(delimiter, pos_start)) != string::npos) {
        token = s.substr (pos_start, pos_end - pos_start);
        pos_start = pos_end + delim_len;
        res.push_back (token);
    }

    res.push_back (s.substr (pos_start));
    return res;
}

class Data {
private:
    map<pair<unsigned int, unsigned int>, set<pair<unsigned int, unsigned int>>> SZDG_table;
    unsigned int S_max = 0, Z_max = 0, D_max = 0, G_max = 0;
    string Y_type, Y_family, Y_link;
    string func_family, func_link;
    vector<pair<string, string>> parameters;
    vector<tuple<string, string, vector<double>>> prior;
public:
    Data(const string& file_name) {
        fstream input(file_name);
        string ctrl;
        while (input >> ctrl) {
            if (ctrl == "SZDG") {
                unsigned int s, z, d, g;
                input >> s >> z >> d >> g;
                SZDG_table[make_pair(z, d)].insert(make_pair(s, g));
                S_max = max(S_max, s);
                Z_max = max(Z_max, z);
                D_max = max(D_max, d);
                G_max = max(G_max, g);
            }
            else if (ctrl == "Y") {
                input >>this->Y_family >> this->Y_link;
            }
            else if (ctrl == "prior") {
                string type, func;
                input >> type >> func;
                vector<double> tmp_param;
                int n;
                input >> n;
                for (int i = 0; i < n; ++i){
                    double param;
                    input >> param;
                    tmp_param.push_back(param);
                }
                prior.emplace_back(type, func, tmp_param);
            }
        }
        input.close();
        input = fstream("family_info.txt");
        string type, family, family_func, link, link_func;
        int param_count;
        while (input >> family >> type >> family_func >> param_count) {
            for (int k = 0; k < param_count; ++k) {
                string param_name, param_type;
                input >> param_name >> param_type;
                if (family == Y_family)
                    parameters.emplace_back(param_name, param_type);
            }
            if (family == Y_family) {
                func_family = family_func;
                Y_type = type;
                break;
            }
        }
        input.close();

        input = fstream("link_info.txt");
        while (input >> family >> link >> link_func) {
            if (family == Y_family && link == Y_link) {
                func_link = link_func;
                break;
            }
        }
        input.close();
    }

    string to_stan_functions() const {
        fstream input = fstream("function_implement.txt");
        string str;
        bool aim = false;
        for (string line; getline(input, line); ){
            if (line == "<<<>>>") aim = false;
            if (!aim) {
                auto splitted = split(line, " ");
                if (splitted[0] == "<<<") {
                    string func_name = splitted[1];
                    if (func_name == func_family || func_name == func_link) {
                        aim = true;
                    }
                }
                continue;
            }
            else {
                str += "    " + line + "\n";
            }
        }
        input.close();
        if (!str.empty())
            str = "functions {\n" + str + "}\n";
        return str;
    }

    string to_stan_data() const {
        string str;
        str += "data {\n";
        str += "    int<lower=0> N;\n";
        str += "    int<lower=0> PS;\n";
        str += "    int<lower=0> PG;\n";
        str += "    int<lower=0, upper=" + to_string(Z_max) + "> Z[N];\n";
        str += "    int<lower=0, upper=" + to_string(D_max) + "> D[N];\n";
        string Y_str;
        if (Y_type == "real") Y_str = "real Y[N];";
        else if (Y_type == "positive") Y_str = "real<lower=0> Y[N];";
        else if (Y_type == "binary") Y_str = "int<lower=0, upper=1> Y[N];";
        else if (Y_type == "count") Y_str = "int<lower=0> Y[N];";
        str += "    " + Y_str + "\n";
        str += "    matrix[N, PS] XS;\n";
        str += "    matrix[N, PG] XG;\n";
        str += "}\n";
        return (str);
    };

    string to_stan_transformed_data() const {
        string str;
        str += "transformed data {\n";
        str += "    int S[" + to_string(G_max + 1) + "];\n";
        set<pair<unsigned int, unsigned int>> tmpset;
        for (auto zdsg: SZDG_table) {
            for (auto sg : zdsg.second) {
                tmpset.insert(sg);
            }
        }
        for (auto sg: tmpset){
            unsigned int s = sg.first, g = sg.second;
            str += "    S[" + to_string(g + 1) + "] = " + to_string(s + 1) + ";\n";
        }
        str += "}\n";
        return (str);
    }
    string to_stan_parameters() const {
        string S_str = to_string(S_max); // always zero for S == 0, so omit that.
        string G_str = to_string(1 + G_max);
        string str;
        str += "parameters {\n";
        str += "    matrix[" + S_str + ", PS] beta_S;\n";
        str += "    matrix[" + G_str + ", PG] beta_G;\n";
        for (auto &p : parameters) {
            string type_str;
            if (p.second == "real") type_str = "real";
            else if (p.second == "positive") type_str = "real<lower=0>";
            str += "    " + type_str + " " + p.first + "[" + G_str + "];\n";
        }
        str += "}\n";
        return str;
    }

    string to_stan_model() const {
        string str_sp1 = to_string(S_max + 1);
        string str;
        str += "model {\n";

        // prior
        for (auto& prior_info : prior) {
            string type = get<0>(prior_info);
            string func = get<1>(prior_info);
            vector<double> params = get<2>(prior_info);
            string params_str;
            for (auto param: params) {
                if (!params_str.empty())
                    params_str += ", ";
                params_str += to_string(param);
            }
            if (func == "uniform")
                continue;
            if (type == "intercept") {
                str += "    beta_S[:, 1] ~ " + func + "(" + params_str + ");\n";
                str += "    beta_G[:, 1] ~ " + func + "(" + params_str + ");\n";
            }
            else if (type == "coefficient") {
                str += "    to_vector(beta_S[:, 2:PS]) ~ " + func + "(" + params_str + ");\n";
                str += "    to_vector(beta_G[:, 2:PG]) ~ " + func + "(" + params_str + ");\n";
            }
            else {
                bool found = false;
                for (auto& param : parameters)
                    if (type == param.first) {
                        found = true;
                        break;
                    }
                if (found) {
                    str += "    " + type + " ~ " + func + "(" + params_str + ");\n";
                }
            }
        }

        str += "    for (n in 1:N) {\n";
        str += "        int length;\n";
        str += "        real log_prob[" + str_sp1 + "];\n";
        str += "        log_prob[1] = 0;\n";
        str += "        for (s in 2:" + str_sp1 + ") {\n";
        str += "            log_prob[s] = XS[n] * beta_S[s - 1]';\n";
        str += "        }\n";
        bool b_else = false;
        for (auto &p : SZDG_table) {
            auto z = p.first.first, d = p.first.second;
            auto sg_set = p.second;
            str += "        " + (b_else?string("else "):string("")) + "if (" +
                    "Z[n] == " + to_string(z) + " && D[n] == " + to_string(d) + ")\n";
            str += "            length = " + to_string(sg_set.size()) + ";\n";
            b_else = true;
        }
        str += "        {\n";
        str += "            real log_l[length];\n";
        b_else = false;
        for (auto &p : SZDG_table) {
            auto z = p.first.first, d = p.first.second;
            auto sg_set = p.second;
            str += "            " + (b_else?string("else "):string("")) + "if (" +
                   "Z[n] == " + to_string(z) + " && D[n] == " + to_string(d) + ") {\n";
            str += "                // strata:";
            for (auto &sg : sg_set) str += " " + to_string(sg.first);
            str += "\n";
            int i = 0;
            for (auto &sg : sg_set) {
                unsigned int s = sg.first, g = sg.second;
                string str_param;
                for (auto &p : parameters) {
                    str_param += ", " + p.first + "[" + to_string(g + 1) + "]";
                }
                str += "                log_l[" + to_string(++i) + "] = log_prob[" +
                        to_string(s + 1) + "] + " +
                        func_family + "(Y[n] | " + func_link + "(" +
                        "XG[n] * beta_G[" + to_string(g + 1) + "]')" +
                        str_param + ");\n";
            }
            str += "            }\n";
            b_else = true;
        }
        str += "            target += log_sum_exp(log_l) - log_sum_exp(log_prob);\n";
        str += "        }\n";
        str += "    }\n";
        str += "}\n";
        return str;
    }
    string to_stan_generated_quantities() const {
        string S_str = to_string(S_max + 1);
        string G_str = to_string(G_max + 1);
        string str = "generated quantities {\n";
        str += "    vector[" + G_str + "] mean_effect;\n";
        str += "    {\n";
        str += "        matrix[N, " + G_str + "] expected_mean = XG * beta_G';\n";
        str += "        matrix[N, " + S_str + "] log_prob;\n";
        str += "        vector[" + S_str + "] denom;\n";
        str += "        vector[" + G_str + "] numer;\n";
        str += "        log_prob[:, 1] = 0 * log_prob[:, 1];\n";
        str += "        log_prob[:, 2:" + S_str + "] = XS * beta_S';\n";
        str += "        for (n in 1:N) {\n";
        str += "            log_prob[n] -= log_sum_exp(log_prob[n]);\n";
        str += "        }\n";
        str += "        for (s in 1:" + S_str + ") denom[s] = mean(exp(log_prob[:, s]));\n";
        str += "        for (g in 1:" + G_str + ") {\n";
        str += "            numer[g] = mean(expected_mean[:, g] .* exp(log_prob[:, S[g]]));\n";
        str += "            mean_effect[g] = numer[g] / denom[S[g]];\n";
        str += "        }\n";
        str += "    }\n";
        str += "}\n";
        return (str);
    }
    void print_info() const {
        cout << S_max << ' ' << Z_max << ' ' << D_max << ' ' << G_max << endl;
        cout << Y_type << ' ' << Y_family << ' ' << Y_link << endl;
    }
};

int main() {
    Data data("try1.txt");
    data.print_info();
    fstream output("try1.stan", std::fstream::out | std::fstream::trunc);
    output << data.to_stan_functions() << endl;
    output << data.to_stan_data() << endl;
    output << data.to_stan_transformed_data() << endl;
    output << data.to_stan_parameters() << endl;
    output << data.to_stan_model() << endl;
    output << data.to_stan_generated_quantities() << endl;
    output.close();
    return 0;
}
