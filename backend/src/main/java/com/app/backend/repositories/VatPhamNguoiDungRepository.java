package com.app.backend.repositories;

import com.app.backend.models.VatPham;
import com.app.backend.models.VatPhamNguoiDung;
import com.app.backend.models.NguoiDung;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface VatPhamNguoiDungRepository extends JpaRepository<VatPhamNguoiDung, Long> {

    /**
     * Lấy inventory của một user
     */
    List<VatPhamNguoiDung> findByNguoiDungId(Long nguoiDungId);

    /**
     * Lấy inventory có số lượng > 0 của user
     */
    @Query("SELECT v FROM VatPhamNguoiDung v WHERE v.nguoiDung.id = :userId AND v.soLuong > 0")
    List<VatPhamNguoiDung> findAvailableByUserId(@Param("userId") Long userId);

    /**
     * Tìm vật phẩm cụ thể của user
     */
    Optional<VatPhamNguoiDung> findByNguoiDungIdAndVatPhamId(Long nguoiDungId, Long vatPhamId);

    /**
     * Tìm theo user và loại vật phẩm
     */
    @Query("SELECT v FROM VatPhamNguoiDung v WHERE v.nguoiDung.id = :userId AND v.vatPham.loai = :loai")
    Optional<VatPhamNguoiDung> findByUserIdAndLoaiVatPham(@Param("userId") Long userId,
                                                          @Param("loai") com.app.backend.models.enums.LoaiVatPham loai);

    /**
     * Giảm số lượng vật phẩm (khi sử dụng)
     */
    @Modifying
    @Query("UPDATE VatPhamNguoiDung v SET v.soLuong = v.soLuong - 1, v.suDungLuc = CURRENT_TIMESTAMP " +
            "WHERE v.nguoiDung.id = :userId AND v.vatPham.id = :vatPhamId AND v.soLuong > 0")
    int decrementQuantity(@Param("userId") Long userId, @Param("vatPhamId") Long vatPhamId);

    /**
     * Tăng số lượng vật phẩm (khi nhận thưởng)
     */
    @Modifying
    @Query("UPDATE VatPhamNguoiDung v SET v.soLuong = v.soLuong + :amount, v.nhanLuc = CURRENT_TIMESTAMP " +
            "WHERE v.nguoiDung.id = :userId AND v.vatPham.id = :vatPhamId")
    int incrementQuantity(@Param("userId") Long userId, @Param("vatPhamId") Long vatPhamId, @Param("amount") int amount);

    /**
     * Đếm tổng số vật phẩm của user
     */
    @Query("SELECT COALESCE(SUM(v.soLuong), 0) FROM VatPhamNguoiDung v WHERE v.nguoiDung.id = :userId")
    int countTotalItemsByUserId(@Param("userId") Long userId);
}
