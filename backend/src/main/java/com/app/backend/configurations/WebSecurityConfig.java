package com.app.backend.configurations;

import com.app.backend.filters.JwtTokenFilter;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.HttpStatusEntryPoint;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.PUT;

@Configuration
//@EnableMethodSecurity
@EnableWebSecurity(debug = true)
@EnableMethodSecurity(prePostEnabled = true)
//@EnableGlobalMethodSecurity(prePostEnabled = true)
@EnableWebMvc
@RequiredArgsConstructor
public class WebSecurityConfig {
    private final JwtTokenFilter jwtTokenFilter;
    @Value("${api.prefix}")
    private String apiPrefix;

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                .addFilterBefore(jwtTokenFilter, UsernamePasswordAuthenticationFilter.class)
                //.cors(Customizer.withDefaults())
                .exceptionHandling(customizer -> customizer.authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED)))
                .sessionManagement(c -> c.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .authorizeHttpRequests(requests -> {
                    requests
                            .requestMatchers(
                                    String.format("%s/users/register", apiPrefix),
                                    String.format("%s/users/login", apiPrefix)
                            )
                            .permitAll()
                            .requestMatchers(GET,
                                    String.format("%s/roles**", apiPrefix)).permitAll()
                            .requestMatchers(PUT,
                                    String.format("%s/users/change-password", apiPrefix)).hasAuthority("ROLE_USER")
                            .requestMatchers(
                                    String.format("%s/users/register", apiPrefix),
                                    String.format("%s/users/login", apiPrefix),
                                    String.format("%s/users/verify-email", apiPrefix),
                                    String.format("%s/users/resend-verification", apiPrefix)
                            ).permitAll()
                            .requestMatchers(GET,
                                    String.format("%s/users/profile-images/**", apiPrefix))
                            .permitAll()
//
//                            .requestMatchers(GET,
//                                    String.format("%s/policies/**", apiPrefix)).permitAll()
//
//                            .requestMatchers(GET,
//                                    String.format("%s/categories/**", apiPrefix)).permitAll()
//
//                            .requestMatchers(GET,
//                                    String.format("%s/products/**", apiPrefix)).permitAll()
//
//                            .requestMatchers(GET,
//                                    String.format("%s/products/images/*", apiPrefix)).permitAll()
//
//                            .requestMatchers(GET,
//                                    String.format("%s/orders/**", apiPrefix)).permitAll()
//                            .requestMatchers(GET,
//                                    String.format("%s/users/profile-images/**", apiPrefix))
//                            .permitAll()
//
//                            .requestMatchers(GET,
//                                    String.format("%s/order_details/**", apiPrefix)).permitAll()

                            .anyRequest()
                            .authenticated();
                    //.anyRequest().permitAll();
                })
                .csrf(AbstractHttpConfigurer::disable)
        ;
//        http.securityMatcher(String.valueOf(EndpointRequest.toAnyEndpoint()));
        return http.build();
    }
}
