<template>
  <div>
    <div class="todoListwrapper">
      <ul>
        <TodoItem />
      </ul>
    </div>
    <TodoFilters />
  </div>
</template>

<script>
import TodoItem from "../components/TodoItem";
import TodoFilters from "../components/TodoFilters";
export default {
  components: {
    TodoItem, TodoFilters
  },
}
</script>
