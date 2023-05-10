/**
 * @file ast.cpp
 * @brief 为PASCAL-S编译器实现AST节点相关功能
 * @author 滕宇航 胡少卿 胡亦凡 于可欣 陈迁 黄卓琳 
 * @version 1.0
 * @date 2023-05-09
 * 
 * @copyright Copyright (c) 2023  北京邮电大学计算机学院2020211312班
 * 
 */
#include "ast.hpp"
#include <fmt/core.h>
#include <iostream>
#include <sstream>

using namespace spc;
using namespace std;

using NodePtr = std::shared_ptr<AbstractNode>;

/**
 * @brief 打印抽象语法树AST转换后的json格式
 */
void AbstractNode::print_json() const { clog << this->to_json() << endl; }

/**
 * @brief 将抽象语法树转化为JSON格式输出
 * @return string 返回表示转换后的JSON格式的字符串
 */
string AbstractNode::to_json() const {
  std::stringstream ret;
  ret << "{";
  ret << this->json_head();
  if (this->should_have_children()) {
    ret << ", \"children\": [";
    bool is_first = true;
    for (auto &node : this->_children) {
      if (is_first)
        is_first = false;
      else
        ret << ", ";
      ret << node->to_json();
    }
    ret << "]";
  }
  ret << "}";
  return ret.str();
}
/**
 * @brief SysCallNode类的头部信息
 * @return string 返回SysCallNode类的头部信息，表示为JSON格式的字符串
 */
string SysCallNode::json_head() const {
  return std::string{"\"type\": \"SysCall\", \"identifier\": \""} + to_string(this->routine->routine) +
         "\", \"args\": " + this->args->to_json();
}
/**
 * @brief 构造函数，用于初始化SysCallNode对象
 * @param routine SysRoutineNode类型的子节点
 * @param args ArgListNode类型的子节点
 */
SysCallNode::SysCallNode(const NodePtr &routine, const NodePtr &args)
    : routine(cast_node<SysRoutineNode>(routine)), args(cast_node<ArgListNode>(args)) {}
/**
 * @brief 将Type类型转化为string类型
 * @param type 待转化的Type类型
 * @return std::string 返回转化后的字符串
 */
std::string spc::type2string(Type type) {
  const std::map<Type, std::string> type_to_string{{Type::UNDEFINED, "<undefined-type>"},
                                                   {Type::STRING, "string"},
                                                   {Type::INTEGER, "integer"},
                                                   {Type::REAL, "real"},
                                                   {Type::BOOLEN, "boolen"},
                                                   {Type::CHAR, "char"},
                                                   {Type::ARRAY, "array"},
                                                   {Type::STRUCT, "struct"},
                                                   {Type::VOID, "void"}};
  return type_to_string.at(type);
}
/**
 * @brief SimpleTypeNode类的头部信息
 * @return std::string 返回SimpleTypeNode类的头部信息，表示为JSON格式的字符串
 */
std::string SimpleTypeNode::json_head() const {
  return fmt::format("\"type\": \"Type\", \"name\":\"{}\"", type2string(this->type));
}
/**
 * @brief StringTypeNode类的头部信息
 * @return std::string 返回StringTypeNode类的头部信息，表示为JSON格式的字符串
 */
std::string StringTypeNode::json_head() const {
  return fmt::format("\"type\": \"Type\", \"name\":\"{}\"", type2string(this->type));
}
/**
 * @brief ArrayTypeNode类的头部信息
 * @return std::string 返回ArrayTypeNode类的头部信息，表示为JSON格式的字符串
 */
std::string ArrayTypeNode::json_head() const {
  return fmt::format("\"type\": \"Type\", \"name\":\"{}\"", type2string(this->type));
}

/**
 * @brief RecordTypeNode类的头部信息
 * @return std::string 返回RecordTypeNode类的头部信息，表示为JSON格式的字符串
 */
std::string RecordTypeNode::json_head() const {
  return fmt::format("\"type\": \"Type\", \"name\":\"{}\"", type2string(this->type));
}
/**
 * @brief 返回Array类型节点的JSON格式头部
 * @return std::string Array类型节点的JSON格式头部
 */
std::string AliasTypeNode::json_head() const {
  return fmt::format("\"type\": \"Type\", \"name\": \"alias\", \"identifier\":{{{0}}}", this->identifier->to_json());
}
