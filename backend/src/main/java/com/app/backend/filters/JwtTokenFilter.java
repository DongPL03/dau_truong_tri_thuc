package com.app.backend.filters;

import com.app.backend.components.JwtTokenUtils;
import com.app.backend.models.NguoiDung;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.util.Pair;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

@Component
@RequiredArgsConstructor
public class JwtTokenFilter extends OncePerRequestFilter {
    @Value("${api.prefix}")
    private String apiPrefix;
    private final UserDetailsService userDetailsService;
    private final JwtTokenUtils jwtTokenUtil;

    //    @Override
//    protected void doFilterInternal(@NonNull HttpServletRequest request,
//                                    @NonNull HttpServletResponse response,
//                                    @NonNull FilterChain filterChain) throws IOException {
//        try {
//            String path = request.getRequestURI();
//            if (path.startsWith("/ws") || path.startsWith("/topic") || path.startsWith("/app")) {
//                filterChain.doFilter(request, response);
//                return;
//            }
//
//            if (isBypassToken(request)) {
//                filterChain.doFilter(request, response); //enable bypass
//                return;
//            }
//            final String authHeader = request.getHeader("Authorization");
//            if (authHeader == null || !authHeader.startsWith("Bearer ")) {
//                response.sendError(
//                        HttpServletResponse.SC_UNAUTHORIZED,
//                        "authHeader null or not started with Bearer");
//                return;
//            }
//            final String token = authHeader.substring(7);
//            final String phoneNumber = jwtTokenUtil.getSubject(token);
//            if (phoneNumber != null
//                    && SecurityContextHolder.getContext().getAuthentication() == null) {
//                NguoiDung userDetails = (NguoiDung) userDetailsService.loadUserByUsername(phoneNumber);
//                if (jwtTokenUtil.validateToken(token, userDetails)) {
////                    UsernamePasswordAuthenticationToken authenticationToken =
////                            new UsernamePasswordAuthenticationToken(
////                                    userDetails,
////                                    null,
////                                    userDetails.getAuthorities()
////                            );
//                    List<String> roles = jwtTokenUtil.extractRoles(token);
//                    List<SimpleGrantedAuthority> authorities = roles.stream()
//                            .map(SimpleGrantedAuthority::new)
//                            .toList();
//                    UsernamePasswordAuthenticationToken authenticationToken =
//                            new UsernamePasswordAuthenticationToken(userDetails, null, authorities);
//                    authenticationToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
//                    SecurityContextHolder.getContext().setAuthentication(authenticationToken);
//                }
//            }
//            filterChain.doFilter(request, response); //enable bypass
//        } catch (Exception e) {
//            //response.sendError(HttpServletResponse.SC_UNAUTHORIZED, e.getMessage());
//            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
//            response.getWriter().write(e.getMessage());
//        }
//
//    }
    @Override
    protected void doFilterInternal(@NonNull HttpServletRequest request,
                                    @NonNull HttpServletResponse response,
                                    @NonNull FilterChain filterChain) throws IOException {
        try {
            String path = request.getRequestURI();

            // ✅ 1. Bỏ qua request SockJS "info" (handshake đầu tiên)
            if (path.equals("/ws/info") || path.startsWith("/ws/info")) {
                filterChain.doFilter(request, response);
                return;
            }

            // ✅ 2. Cho phép toàn bộ WebSocket / STOMP endpoints
            if (path.startsWith("/ws") || path.startsWith("/topic") || path.startsWith("/app")) {

                // Thử đọc token từ query param nếu có (?token=Bearer%20xxxxx)
                String queryToken = request.getParameter("token");
                if (queryToken != null && queryToken.startsWith("Bearer ")) {
                    String token = queryToken.substring(7);
                    try {
                        String phoneNumber = jwtTokenUtil.getSubject(token);
                        if (phoneNumber != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                            NguoiDung user = (NguoiDung) userDetailsService.loadUserByUsername(phoneNumber);
                            if (jwtTokenUtil.validateToken(token, user)) {
                                List<String> roles = jwtTokenUtil.extractRoles(token);
                                List<SimpleGrantedAuthority> authorities = roles.stream()
                                        .map(SimpleGrantedAuthority::new)
                                        .toList();

                                UsernamePasswordAuthenticationToken auth =
                                        new UsernamePasswordAuthenticationToken(user, null, authorities);
                                auth.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                                SecurityContextHolder.getContext().setAuthentication(auth);
                            }
                        }
                    } catch (Exception e) {
                        // Không chặn WS nếu token sai, chỉ log cảnh báo
                        System.out.println("Warning: Invalid WebSocket token - " + e.getMessage());
                    }
                }

                // Luôn cho phép WebSocket request tiếp tục (kể cả không có token)
                filterChain.doFilter(request, response);
                return;
            }

            // ✅ 3. Bỏ qua các API công khai trong hệ thống
            if (isBypassToken(request)) {
                filterChain.doFilter(request, response);
                return;
            }

            // ✅ 4. Đọc token từ Header (Bearer)
            String token = null;
            String authHeader = request.getHeader("Authorization");
            if (authHeader != null && authHeader.startsWith("Bearer ")) {
                token = authHeader.substring(7);
            }

            // ✅ 5. Không có token header → chặn (chỉ với API)
            if (token == null) {
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Missing or invalid Authorization token");
                return;
            }

            // ✅ 6. Giải mã & xác thực token
            String phoneNumber = jwtTokenUtil.getSubject(token);
            if (phoneNumber != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                NguoiDung userDetails = (NguoiDung) userDetailsService.loadUserByUsername(phoneNumber);

                if (jwtTokenUtil.validateToken(token, userDetails)) {
                    List<String> roles = jwtTokenUtil.extractRoles(token);
                    List<SimpleGrantedAuthority> authorities = roles.stream()
                            .map(SimpleGrantedAuthority::new)
                            .toList();

                    UsernamePasswordAuthenticationToken authenticationToken =
                            new UsernamePasswordAuthenticationToken(userDetails, null, authorities);
                    authenticationToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

                    SecurityContextHolder.getContext().setAuthentication(authenticationToken);
                }
            }

            // ✅ 7. Cho phép request tiếp tục
            filterChain.doFilter(request, response);

        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.getWriter().write("Unauthorized: " + e.getMessage());
        }
    }


    private boolean isBypassToken(@NonNull HttpServletRequest request) {
        final List<Pair<String, String>> bypassTokens = Arrays.asList(
                // Healthcheck request, no JWT token required
                Pair.of(String.format("%s/roles**", apiPrefix), "GET"),

                Pair.of(String.format("%s/tranDau**", apiPrefix), "GET"),
                Pair.of(String.format("%s/chuDe**", apiPrefix), "GET"),

                Pair.of(String.format("%s/users/register", apiPrefix), "POST"),
                Pair.of(String.format("%s/users/login", apiPrefix), "POST"),
                Pair.of(String.format("%s/users/profile-images/**", apiPrefix), "GET"),
                Pair.of(String.format("%s/users/refreshToken", apiPrefix), "POST"),

                Pair.of(String.format("%s/users/verify-email", apiPrefix), "GET"),
                Pair.of(String.format("%s/users/idVaiTro/**", apiPrefix), "GET"),
                Pair.of(String.format("%s/users/resend-verification", apiPrefix), "POST"),

                Pair.of(String.format("%s/cauHoi/media/**", apiPrefix), "GET"),
                Pair.of(String.format("%s/provinces/**", apiPrefix), "GET")
        );

        String requestPath = request.getServletPath();
        String requestMethod = request.getMethod();

        for (Pair<String, String> token : bypassTokens) {
            String path = token.getFirst();
            String method = token.getSecond();
            // Check if the request path and method match any pair in the bypassTokens list
            if (requestPath.matches(path.replace("**", ".*"))
                    && requestMethod.equalsIgnoreCase(method)) {
                return true;
            }
        }
        return false;
    }
}
