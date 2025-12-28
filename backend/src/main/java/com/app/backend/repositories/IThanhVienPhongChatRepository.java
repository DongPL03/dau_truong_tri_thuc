package com.app.backend.repositories;

import com.app.backend.models.ThanhVienPhongChat;
import com.app.backend.models.enums.VaiTroPhongChat;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

@Repository
public interface IThanhVienPhongChatRepository extends JpaRepository<ThanhVienPhongChat, Long> {

    /**
     * Tìm thành viên trong phòng chat
     */
    Optional<ThanhVienPhongChat> findByPhongChatIdAndNguoiDungId(Long phongChatId, Long nguoiDungId);

    /**
     * Tìm tất cả thành viên đang hoạt động trong phòng chat
     */
    @Query("SELECT tv FROM ThanhVienPhongChat tv " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.daRoi = false")
    List<ThanhVienPhongChat> findActiveByPhongChatId(@Param("phongChatId") Long phongChatId);

    /**
     * Kiểm tra người dùng có trong phòng chat không
     */
    @Query("SELECT CASE WHEN COUNT(tv) > 0 THEN true ELSE false END " +
           "FROM ThanhVienPhongChat tv " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.nguoiDung.id = :nguoiDungId " +
           "AND tv.daRoi = false")
    boolean existsActiveByPhongChatIdAndNguoiDungId(@Param("phongChatId") Long phongChatId,
                                                     @Param("nguoiDungId") Long nguoiDungId);

    /**
     * Đếm số thành viên hoạt động trong phòng chat
     */
    @Query("SELECT COUNT(tv) FROM ThanhVienPhongChat tv " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.daRoi = false")
    int countActiveByPhongChatId(@Param("phongChatId") Long phongChatId);

    /**
     * Tìm admin của phòng chat
     */
    @Query("SELECT tv FROM ThanhVienPhongChat tv " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.vaiTro = 'ADMIN' " +
           "AND tv.daRoi = false")
    List<ThanhVienPhongChat> findAdminsByPhongChatId(@Param("phongChatId") Long phongChatId);

    /**
     * Cập nhật thời điểm đọc tin nhắn
     */
    @Modifying
    @Query("UPDATE ThanhVienPhongChat tv " +
           "SET tv.docCuoiLuc = :docLuc " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.nguoiDung.id = :nguoiDungId")
    void updateDocCuoiLuc(@Param("phongChatId") Long phongChatId,
                          @Param("nguoiDungId") Long nguoiDungId,
                          @Param("docLuc") Instant docLuc);

    /**
     * Thay đổi vai trò thành viên
     */
    @Modifying
    @Query("UPDATE ThanhVienPhongChat tv " +
           "SET tv.vaiTro = :vaiTro " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.nguoiDung.id = :nguoiDungId")
    void updateVaiTro(@Param("phongChatId") Long phongChatId,
                      @Param("nguoiDungId") Long nguoiDungId,
                      @Param("vaiTro") VaiTroPhongChat vaiTro);

    /**
     * Ghim/bỏ ghim phòng chat
     */
    @Modifying
    @Query("UPDATE ThanhVienPhongChat tv " +
           "SET tv.daGhim = :daGhim " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.nguoiDung.id = :nguoiDungId")
    void updateDaGhim(@Param("phongChatId") Long phongChatId,
                      @Param("nguoiDungId") Long nguoiDungId,
                      @Param("daGhim") Boolean daGhim);

    /**
     * Tắt/bật thông báo
     */
    @Modifying
    @Query("UPDATE ThanhVienPhongChat tv " +
           "SET tv.daTatThongBao = :daTat " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.nguoiDung.id = :nguoiDungId")
    void updateDaTatThongBao(@Param("phongChatId") Long phongChatId,
                             @Param("nguoiDungId") Long nguoiDungId,
                             @Param("daTat") Boolean daTat);

    /**
     * Lấy danh sách ID người dùng trong phòng chat (không bao gồm người gửi)
     */
    @Query("SELECT tv.nguoiDung.id FROM ThanhVienPhongChat tv " +
           "WHERE tv.phongChat.id = :phongChatId " +
           "AND tv.nguoiDung.id != :excludeUserId " +
           "AND tv.daRoi = false")
    List<Long> findUserIdsExcluding(@Param("phongChatId") Long phongChatId,
                                    @Param("excludeUserId") Long excludeUserId);
}
