#pragma once

class ArenaAllocator {
public:
    explicit ArenaAllocator(size_t bytes) : m_size(bytes) {
        m_buffer = (std::byte*)malloc(m_size);
        m_offset = m_buffer;
    }

    template<typename T>
    T* alloc() {
        void* offset = m_offset;
        m_offset += sizeof(T);
        return static_cast<T*>(offset);
    }

    ArenaAllocator(const ArenaAllocator&) = delete;
    ArenaAllocator& operator=(const ArenaAllocator&) = delete;

    ~ArenaAllocator() {
        free(m_buffer);
    }

private:
    size_t m_size;
    std::byte* m_buffer;
    std::byte* m_offset;
};