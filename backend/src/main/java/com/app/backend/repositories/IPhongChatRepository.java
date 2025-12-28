package com.app.backend.repositories;

import com.app.backend.models.PhongChat;
import com.app.backend.models.enums.LoaiPhongChat;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IPhongChatRepository extends JpaRepository<PhongChat, Long> {

    /**
     * Tìm tất cả phòng chat của người dùng
     */
    @Query("SELECT DISTINCT pc FROM PhongChat pc " +
           "JOIN pc.thanhVien tv " +
           "WHERE tv.nguoiDung.id = :nguoiDungId " +
           "AND tv.daRoi = false " +
           "AND pc.daXoa = false " +
           "ORDER BY pc.thoiGianTinNhanCuoi DESC NULLS LAST, pc.capNhatLuc DESC")
    Page<PhongChat> findByNguoiDungId(@Param("nguoiDungId") Long nguoiDungId, Pageable pageable);

    /**
     * Tìm các phòng chat được ghim của người dùng
     */
    @Query("SELECT DISTINCT pc FROM PhongChat pc " +
           "JOIN pc.thanhVien tv " +
           "WHERE tv.nguoiDung.id = :nguoiDungId " +
           "AND tv.daGhim = true " +
           "AND tv.daRoi = false " +
           "AND pc.daXoa = false " +
           "ORDER BY pc.thoiGianTinNhanCuoi DESC NULLS LAST")
    List<PhongChat> findPinnedByNguoiDungId(@Param("nguoiDungId") Long nguoiDungId);

    /**
     * Tìm phòng chat 1-1 giữa 2 người dùng
     */
    @Query("SELECT pc FROM PhongChat pc " +
           "JOIN pc.thanhVien tv1 " +
           "JOIN pc.thanhVien tv2 " +
           "WHERE pc.loai = 'DON' " +
           "AND tv1.nguoiDung.id = :nguoiDung1Id " +
           "AND tv2.nguoiDung.id = :nguoiDung2Id " +
           "AND tv1.daRoi = false " +
           "AND tv2.daRoi = false " +
           "AND pc.daXoa = false")
    Optional<PhongChat> findPrivateChat(@Param("nguoiDung1Id") Long nguoiDung1Id, 
                                         @Param("nguoiDung2Id") Long nguoiDung2Id);

    /**
     * Đếm tin nhắn chưa đọc trong phòng chat
     */
    @Query("SELECT COUNT(tn) FROM TinNhanPhongChat tn " +
           "JOIN ThanhVienPhongChat tv ON tv.phongChat.id = tn.phongChat.id " +
           "WHERE tn.phongChat.id = :phongChatId " +
           "AND tv.nguoiDung.id = :nguoiDungId " +
           "AND tn.guiLuc > tv.docCuoiLuc " +
           "AND tn.guiBoi.id != :nguoiDungId " +
           "AND tn.daXoa = false")
    long countUnreadMessages(@Param("phongChatId") Long phongChatId, 
                             @Param("nguoiDungId") Long nguoiDungId);

    /**
     * Đếm tổng số tin nhắn chưa đọc của người dùng
     */
    @Query("SELECT COUNT(tn) FROM TinNhanPhongChat tn " +
           "JOIN ThanhVienPhongChat tv ON tv.phongChat.id = tn.phongChat.id " +
           "WHERE tv.nguoiDung.id = :nguoiDungId " +
           "AND tv.daRoi = false " +
           "AND tn.guiLuc > tv.docCuoiLuc " +
           "AND tn.guiBoi.id != :nguoiDungId " +
           "AND tn.daXoa = false")
    long countTotalUnreadMessages(@Param("nguoiDungId") Long nguoiDungId);

    /**
     * Tìm các phòng chat theo loại
     */
    @Query("SELECT pc FROM PhongChat pc " +
           "JOIN pc.thanhVien tv " +
           "WHERE tv.nguoiDung.id = :nguoiDungId " +
           "AND pc.loai = :loai " +
           "AND tv.daRoi = false " +
           "AND pc.daXoa = false " +
           "ORDER BY pc.thoiGianTinNhanCuoi DESC NULLS LAST")
    Page<PhongChat> findByNguoiDungIdAndLoai(@Param("nguoiDungId") Long nguoiDungId,
                                              @Param("loai") LoaiPhongChat loai,
                                              Pageable pageable);

    /**
     * Tìm kiếm phòng chat theo tên hoặc tên thành viên
     */
    @Query("SELECT DISTINCT pc FROM PhongChat pc " +
           "JOIN pc.thanhVien tv " +
           "WHERE tv.nguoiDung.id = :nguoiDungId " +
           "AND tv.daRoi = false " +
           "AND pc.daXoa = false " +
           "AND (LOWER(pc.ten) LIKE LOWER(CONCAT('%', :keyword, '%')) " +
           "    OR EXISTS (SELECT 1 FROM ThanhVienPhongChat tv2 " +
           "               WHERE tv2.phongChat.id = pc.id " +
           "               AND tv2.nguoiDung.id != :nguoiDungId " +
           "               AND LOWER(tv2.nguoiDung.hoTen) LIKE LOWER(CONCAT('%', :keyword, '%'))))")
    Page<PhongChat> searchByKeyword(@Param("nguoiDungId") Long nguoiDungId,
                                    @Param("keyword") String keyword,
                                    Pageable pageable);
}
