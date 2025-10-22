package com.app.backend.services.nguoidung;

import com.app.backend.dtos.UpdateUserDTO;
import com.app.backend.dtos.UserDTO;
import com.app.backend.dtos.UserLoginDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.InvalidPasswordException;
import com.app.backend.models.NguoiDung;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;

public interface INguoiDungService {
    NguoiDung createUser(UserDTO userDTO) throws Exception;

    String login(UserLoginDTO userLoginDT) throws Exception;

    NguoiDung getUserDetailsFromToken(String token) throws Exception;

    NguoiDung getUserDetailsFromRefreshToken(String token) throws Exception;

    NguoiDung updateUser(Long userId, UpdateUserDTO updatedUserDTO) throws Exception;

    Page<NguoiDung> findAll(String keyword, Pageable pageable) throws Exception;

    void resetPassword(Long userId, String newPassword)
            throws InvalidPasswordException, DataNotFoundException;

    void blockOrEnable(Long userId, Boolean active) throws DataNotFoundException;

    void changeProfileImage(Long userId, String imageName) throws Exception;
//    String loginSocial(UserLoginDTO userLoginDTO) throws Exception;

    // add
    void changePasswordWithOldCheck(Long userId, String oldPassword, String newPassword) throws Exception;

    void updateRole(Long userId, Long roleId) throws Exception;

    void softDeleteUser(Long userId) throws Exception;

    void restoreUser(Long userId) throws Exception;

    List<NguoiDung> findByDeletedFalse();

    Optional<NguoiDung> findByIdAndDeletedFalse(Long id);

    Optional<NguoiDung> findByEmailAndDeletedFalse(String email);

}
