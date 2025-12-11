package com.app.backend.repositories;

import com.app.backend.models.TinNhan;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface ITinNhanRepository extends JpaRepository<TinNhan, Long> {

    /**
     * 🔹 Lấy lịch sử chat 1-1 giữa 2 user (chỉ private chat, không tính chat trong trận)
     * - Sắp xếp: mới → cũ (DESC)
     */
    @Query("""
            SELECT t FROM TinNhan t
            WHERE (
              (t.guiBoi.id = :u1 AND t.nhanBoi.id = :u2)
              OR
              (t.guiBoi.id = :u2 AND t.nhanBoi.id = :u1)
            )
            AND t.tranDau IS NULL
            ORDER BY t.guiLuc DESC
            """)
    Page<TinNhan> findPrivateConversation(
            @Param("u1") Long user1Id,
            @Param("u2") Long user2Id,
            Pageable pageable
    );

    /**
     * 🔹 Lấy "inbox" của currentUser:
     * - Mỗi cuộc hội thoại 1-1 với 1 user khác chỉ lấy TIN NHẮN MỚI NHẤT.
     * - Chỉ tính private chat (tranDau IS NULL).
     * - Sắp xếp: cuộc hội thoại mới nhắn gần nhất nằm trên.
     * <p>
     * Ý tưởng:
     * - Lọc tất cả tin nhắn mà currentUser là người gửi hoặc người nhận
     * - Với mỗi cặp (A, B), lấy message có guiLuc lớn nhất
     */
    @Query("""
            SELECT t FROM TinNhan t
            WHERE t.tranDau IS NULL
              AND (t.guiBoi.id = :currentUserId OR t.nhanBoi.id = :currentUserId)
              AND t.guiLuc = (
                SELECT MAX(t2.guiLuc) FROM TinNhan t2
                WHERE t2.tranDau IS NULL
                  AND (
                    (t2.guiBoi.id = t.guiBoi.id AND t2.nhanBoi.id = t.nhanBoi.id)
                    OR
                    (t2.guiBoi.id = t.nhanBoi.id AND t2.nhanBoi.id = t.guiBoi.id)
                  )
              )
            ORDER BY t.guiLuc DESC
            """)
    Page<TinNhan> findLatestInbox(
            @Param("currentUserId") Long currentUserId,
            Pageable pageable
    );
}
