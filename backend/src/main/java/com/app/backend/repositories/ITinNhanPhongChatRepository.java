package com.app.backend.repositories;

import com.app.backend.models.TinNhanPhongChat;
import com.app.backend.models.enums.LoaiTinNhan;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

@Repository
public interface ITinNhanPhongChatRepository extends JpaRepository<TinNhanPhongChat, Long> {

    /**
     * Lấy tin nhắn trong phòng chat, sắp xếp mới nhất trước
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc DESC")
    Page<TinNhanPhongChat> findByPhongChatId(@Param("phongChatId") Long phongChatId, Pageable pageable);

    /**
     * Lấy tin nhắn trong phòng chat, sắp xếp cũ nhất trước (để hiển thị chat)
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc ASC")
    Page<TinNhanPhongChat> findByPhongChatIdAsc(@Param("phongChatId") Long phongChatId, Pageable pageable);

    /**
     * Lấy tin nhắn mới hơn một thời điểm (để load thêm tin nhắn mới)
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.guiLuc > :after " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc ASC")
    List<TinNhanPhongChat> findByPhongChatIdAndGuiLucAfter(@Param("phongChatId") Long phongChatId,
                                                           @Param("after") Instant after);

    /**
     * Lấy tin nhắn cũ hơn một thời điểm (để load thêm tin nhắn cũ)
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.guiLuc < :before " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc DESC")
    Page<TinNhanPhongChat> findByPhongChatIdAndGuiLucBefore(@Param("phongChatId") Long phongChatId,
                                                            @Param("before") Instant before,
                                                            Pageable pageable);

    /**
     * Tìm tin nhắn được ghim
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.daGhim = true " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc DESC")
    List<TinNhanPhongChat> findPinnedByPhongChatId(@Param("phongChatId") Long phongChatId);

    /**
     * Tìm kiếm tin nhắn theo nội dung
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.loai = 'VAN_BAN' " +
           "AND LOWER(tn.noiDung) LIKE LOWER(CONCAT('%', :keyword, '%')) " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc DESC")
    Page<TinNhanPhongChat> searchByKeyword(@Param("phongChatId") Long phongChatId,
                                           @Param("keyword") String keyword,
                                           Pageable pageable);

    /**
     * Lấy tin nhắn theo loại
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.loai = :loai " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc DESC")
    Page<TinNhanPhongChat> findByPhongChatIdAndLoai(@Param("phongChatId") Long phongChatId,
                                                     @Param("loai") LoaiTinNhan loai,
                                                     Pageable pageable);

    /**
     * Lấy tin nhắn cuối cùng trong phòng chat
     */
    @Query("SELECT tn FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.daXoa = false " +
           "ORDER BY tn.guiLuc DESC " +
           "LIMIT 1")
    Optional<TinNhanPhongChat> findLastMessageByPhongChatId(@Param("phongChatId") Long phongChatId);

    /**
     * Đếm tin nhắn trong phòng chat
     */
    @Query("SELECT COUNT(tn) FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.daXoa = false")
    long countByPhongChatId(@Param("phongChatId") Long phongChatId);

    /**
     * Xóa mềm tin nhắn
     */
    @Modifying
    @Query("UPDATE TinNhanPhongChat tn " +
           "SET tn.daXoa = true, tn.xoaLuc = :xoaLuc " +
           "WHERE tn.id = :id")
    void softDelete(@Param("id") Long id, @Param("xoaLuc") Instant xoaLuc);

    /**
     * Ghim/bỏ ghim tin nhắn
     */
    @Modifying
    @Query("UPDATE TinNhanPhongChat tn " +
           "SET tn.daGhim = :daGhim " +
           "WHERE tn.id = :id")
    void updateDaGhim(@Param("id") Long id, @Param("daGhim") Boolean daGhim);

    /**
     * Đếm tin nhắn media (ảnh, file) trong phòng chat
     */
    @Query("SELECT COUNT(tn) FROM TinNhanPhongChat tn " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tn.loai IN ('HINH_ANH', 'TAP_TIN', 'AM_THANH') " +
           "AND tn.daXoa = false")
    long countMediaByPhongChatId(@Param("phongChatId") Long phongChatId);
}
